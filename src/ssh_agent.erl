-module(ssh_agent).
-behaviour(gen_server).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

-export([exec/3]).
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%
% external
%
 
start(Params) -> gen_server:start(?MODULE, Params, []).
exec(Agent, Cmd, Params) when is_binary(Cmd) -> exec(Agent, binary_to_list(Cmd), Params);
exec(Agent, Cmd, Params) -> gen_server:cast(Agent, {exec, Cmd, Params}).

%
% gen_server
%

init({Id, Host, Port, User, Password}) ->
    ?PUB({ssh_agent, Id}, exit),
    ?SUB({ssh_agent, Id}), 
    ?AFTER(0, connect),
    {ok, #ssha{ pnode = Id, 
         params = [{host, Host},
                   {port, Port},
                   {user, User},
                   {password, Password}]}}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast({exec, _Cmd, _Params}, State) when State#ssha.con =:= wait_for_connect -> 
    {noreply, State};

handle_cast({exec, Cmd, Params}, State) when State#ssha.con =:= undefined -> 
    ?ERROR("Command ~p with ~p was dropped - no connection", [Cmd, Params]),
    ?AFTER(?SSH_CONNECT_TIMEOUT, connect),
    {noreply, State#ssha{ con = wait_for_connect }};

handle_cast({exec, Cmd, Params}, State) when State#ssha.con =/= undefined -> 
    case ssh_connection:session_channel(State#ssha.con, 
            ?SSH_WINDOW_SIZE, 
            ?SSH_PACKET_SIZE, 
            ?SSH_CHANNEL_TIMEOUT) of
        {ok, Ch} ->
            ssh_connection:exec(State#ssha.con, Ch, Cmd, ?SSH_COMMAND_TIMEOUT),
            {noreply, replace_channel_info(Ch, {Params, []}, State)};
        {error, closed} ->
            ?ERROR("Command ~p with ~p was dropped cause channel was closed", [Cmd, Params]),
            ?PUB({pnode, State#ssha.pnode}, {state, offline}),
            ?AFTER(?SSH_CONNECT_TIMEOUT, connect),
            {noreply, State#ssha{ con = wait_for_connect }};
        Any ->
            ?ERROR("Can't get channel ~p", [Any]),
            {noreply, State#ssha{ con = undefined }}
    end;
handle_cast(Msg, State) -> ?DBG("~p got cast ~p", [?MODULE, Msg]), {noreply, State}.

handle_info(connect, State) when (State#ssha.con =:= undefined) or (State#ssha.con =:= wait_for_connect)  -> 
    PrivDir = wa_app:priv_dir(),
    UserDir = lists:concat([PrivDir, "/ssh"]),
    
    Host = proplists:get_value(host, State#ssha.params, ?DEFAULT_SSH_HOST),
    User = proplists:get_value(user, State#ssha.params, ?DEFAULT_SSH_USER),
    Password = proplists:get_value(password, State#ssha.params, ?DEFAULT_SSH_PASSWORD),
    Port = proplists:get_value(port, State#ssha.params, ?DEFAULT_SSH_PORT),
    
    Options = case Password of
        "" -> [{silently_accept_hosts, true}, 
               {user_interaction, false}, 
               {user_dir, UserDir},
               {user, User}, 
               {connect_timeout, ?SSH_CONNECT_TIMEOUT}];
        _ -> [{silently_accept_hosts, true}, 
               {user_interaction, false}, 
               {user_dir, UserDir},
               {password, Password}, 
               {user, User}, 
               {connect_timeout, ?SSH_CONNECT_TIMEOUT}]
        end,
    case ssh:connect(Host, Port, Options) of
        {ok, Con} ->
            case ssh_connection:session_channel(Con, 
                    ?SSH_WINDOW_SIZE, 
                    ?SSH_PACKET_SIZE, 
                    ?SSH_CONNECT_TIMEOUT) of
                {ok, Ch} -> 
                    case ssh_connection:shell(Con, Ch) of
                        ok ->
                            ?INFO("Connected to ~p", [Host]),
                            ?PUB({pnode, State#ssha.pnode}, {state, online}),
                            {noreply, State#ssha{ con = Con, channels = [{Ch, {term, []}}] }};
                        Any ->
                            ?ERROR("Unable to up the shell ~p", [Any]),
                            {noreply, State#ssha{ con = undefined, channels = [] }}
                    end;
                Any ->
                    ?ERROR("Unable to up the session channel ~p", [Any]),
                    {noreply, State#ssha{ con = undefined, channels = [] }}
            end;
        Any ->
            ?ERROR("Unable to up the connection ~p", [Any]),
            {noreply, State#ssha{ con = undefined, channels = [] }}
    end;

handle_info({ssh_cm, Con, Res}, State) when State#ssha.con =:= Con ->
    [_, Ch | _] = tuple_to_list(Res),
    case proplists:get_value(Ch, State#ssha.channels, undefined) of 
        undefined -> {noreply, State};
        Data -> {noreply, process_ssh_cm(Res, Data, State)}
    end;
handle_info(exit, State) -> {stop, highlander, State};
handle_info(Info, State) -> ?DBG("~p got info ~p", [?MODULE, Info]), {noreply, State}.

terminate(_Reason, State) -> ?PUB({pnode, State#ssha.pnode}, {state, offline}), ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local
%

% All ssh reply types:
% {data, Ch, Stream, Data}
% {eof, Ch}
% {exit_signal, Ch, ExitSignal, ErrorMsg, LanguageString}
% {exit_status, Ch, ExitStatus}

% postprocessed - add data to buff and call fun after end
process_ssh_cm({data, Ch, 0, Data}, {{{postproc, _Fun}, {eachproc, EFun}} = Type, Buff}, S) ->
    EFun(Data), 
    replace_channel_info(Ch, {Type, Buff ++ [Data]}, S);
process_ssh_cm({data, Ch, 1, Data}, {{{postproc, _Fun}, {eachproc, EFun}} = Type, Buff}, S) -> 
    EFun(Data), 
    replace_channel_info(Ch, {Type, Buff ++ [Data]}, S);
process_ssh_cm({eof, Ch}, {{{postproc, Fun}, _}, Buff}, S) -> postproc(S, Ch, Fun, Buff);
process_ssh_cm({exit_signal, Ch, _, _, _}, {{{postproc, Fun}, _}, Buff}, S) -> postproc(S, Ch, Fun, Buff);
process_ssh_cm({exit_status, Ch, _}, {{{postproc, Fun}, _}, Buff}, S) -> postproc(S, Ch, Fun, Buff);

% term
process_ssh_cm({data, Ch, _, Data}, {term, Buff}, S) -> 
    replace_channel_info(Ch, {term, Buff ++ [Data]}, S);

process_ssh_cm({closed, _}, _, S) ->
    ?INFO("Connection closed, reconnect."),
    ?AFTER(?SSH_CONNECT_TIMEOUT, connect),
    S#ssha{ con = wait_for_connect };

process_ssh_cm(Any, {Params, State}, S) ->
    ?ERROR("Undefined ~p for params: ~p and state: ~p", [Any, Params, State]),
    S.

postproc(S, Ch, _Fun, []) ->
    ssh_connection:close(S#ssha.con, Ch),
    delete_channel_info(Ch, S);
postproc(S, Ch, Fun, Buff) ->
    ssh_connection:close(S#ssha.con, Ch),
    Fun(Buff),
    delete_channel_info(Ch, S).

replace_channel_info(K, V, S) -> S#ssha{ channels = [{K, V} | proplists:delete(K, S#ssha.channels)] }.
delete_channel_info(K, S) -> S#ssha{ channels = proplists:delete(K, S#ssha.channels) }.
