-module(pnode).
-behaviour(gen_server).

-export([dummy/2, multiline/2, openvz_list/2, proc_stat/2, split_by_rn/1]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link(?MODULE, Params, []).


init({Id, IP}) ->
    H = shepherd:host_to_string(IP),
    ?SUB(pnodes),
    ?SUB({pnode, Id}),
    ?INFO("Start node with id ~p", [Id]),
    DataId = ets:new(periodic, [set, private, {write_concurrency, true}, {read_concurrency, true}]), 
    ?AFTER(0, check),
    {ok, #pnode{ id = Id, host = H, data = DataId }}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast(Msg, State) -> ?DBG("~p got cast ~p", [?MODULE, Msg]), {noreply, State}.

% from clients
handle_info({info, A}, State) -> full_info_for(A, State), {noreply, State};

handle_info({datasource, _A, Type, _VNode, CId}, State) -> 
    ?WARNING("Call for datasource ~p for console ~p", [Type, CId]),
    {noreply, State};

handle_info({exec, A, Cmds, VNode, CId, SName}, State) -> 
    hipchat_transport:send(lists:concat([
            "Someone start executing '", 
            binary_to_list(SName), 
            "' for hosts ",
            shepherd:host_to_string(State#pnode.host),
            "[", binary_to_list(VNode), "]. Please, wait, don't start it again! (areyoukiddingme)"
        ])),
    {noreply, make_exec(binary:split(Cmds, <<"\n">>, [global]), VNode, {A, CId, SName}, State)};

% from SSH agent
handle_info({state, NS}, State) -> {noreply, broadcast(State#pnode{ state = NS })};
handle_info({ssh, Data}, State) -> {noreply, ssh(Data, State)};
handle_info({exec, Cmds, VNode, AC}, State) -> 
    {noreply, make_exec(Cmds, VNode, AC, State)};

% self
handle_info(check, State) -> ?AFTER(?CHECK_TIMESLICE, check), {noreply, check(State)};
handle_info(Info, State) -> ?DBG("~p got info ~p", [?MODULE, Info]), {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local
%

start_agent(S) ->
    case ssh_agent:start({S#pnode.id, 
                          S#pnode.host, 
                          S#pnode.port, 
                          S#pnode.user,
                          S#pnode.password}) of
        {ok, Agent} -> Agent;
        _Err -> undefined
    end.

broadcast(S) -> ?PUB(clients, {reply, info_msg(S)}), S.
info_msg(S) ->
    StateClass = case S#pnode.state of
        online -> green;
        offline -> red;
        undefined -> black
    end,
    [{id, S#pnode.id}, 
     {ip, shepherd:host_to_json(S#pnode.host)}, 
     {state, S#pnode.state}, 
     {state_class, StateClass},
     {op, <<"info_pnode">>}].

full_info_for(A, S) ->
    A ! {reply, info_msg(S)},
    lists:map(fun(I) -> 
            [Type | _] = tuple_to_list(I),
            case ets:lookup(S#pnode.data, Type) of
                [{_, Data}] -> A ! {reply, [{op, <<"data_pnode">>}, 
                                            {id, S#pnode.id}, 
                                            {ct, Type}, 
                                            {v, Data}]};
                _ -> ok
            end
        end, ?CONFIG(pnode_data, [])). 


ssh({check, Type, Filter, Data}, S) ->
    FData = ?MODULE:Filter(Data, S),
    case ets:lookup(S#pnode.data, Type) of
        [{_, FData}] -> 
            S;
        _Any -> 
            ets:insert(S#pnode.data, {Type, FData}),
            ?PUB(clients, 
                 {reply, [{op, <<"data_pnode">>}, 
                          {id, S#pnode.id}, 
                          {ct, Type}, 
                          {v, FData}]}),
            S
    end;
ssh(Data, S) -> ?INFO("Unknown data from SSH ~p", [Data]), S.

%
% check info
%

check(S) when S#pnode.agent == undefined ->
    broadcast(S#pnode{ state = offline }),
    timer:sleep(?CHECK_TIMESLICE),
    check(S#pnode{ agent = start_agent(S) });
check(S) ->
    NewState = S#pnode{ n = S#pnode.n + 1 },
    lists:map(fun(I) -> req(I, NewState) end, ?CONFIG(pnode_data, [])), 
    NewState.    

req({T, D, C}, S) -> req({T, D, C, dummy}, S);
req({Type, D, Command, Filter}, S) when (S#pnode.n rem D) =:= 0 ->
    Pid = self(),
    ssh_agent:exec(S#pnode.agent, Command, {{postproc, fun(Data) -> 
            Pid ! {ssh, {check, Type, Filter, Data}}
        end}, {eachproc, fun(_) -> ok end}});
req(_, _) -> ok.

%
% filters
%

dummy(D, _S) -> [R | _] = split_by_rn(D), R.

multiline(D, _S) -> split_by_rn(D).

openvz_list(D, _S) ->
    case lists:foldl(fun(I, Acc) -> 
            case re:run(I, "[ ]*([0-9]*)[ ]*([0-9\\-]*)[ ]*([a-z]*)[ ]*([0-9a-f\\.:]*)[ ]*([^ ]*)", 
                    [{capture, all, binary}]) of
                {match, [_, Id, Proc, State, IP, Name]} when Id =/= <<>> ->
                    NP = case Proc of <<"-">> -> 0; _ -> ?BIN2INT(Proc) end,
                    Acc ++ [[{id, Id}, 
                             {proc, NP}, 
                             {state, State},
                             {ip, IP},
                             {name, Name}]];
                _Any -> Acc
            end
        end, [], split_by_rn(D)) of 
            [] -> <<"No nodes was found. Please, install OpenVZ and add some.">>;
            Arr -> Arr
    end.

proc_stat(D, S) ->
    lists:foldl(fun(I, Acc) -> 
            case re:run(I, "^([a-zA-Z0-9]*) ([0-9\\. ]*)$", [{capture, all, list}]) of
                {match, [_, Key, Vals]} ->
                    case re:run(Vals, "([0-9]*)", [global, {capture, all, list}]) of
                        {match, Vs} ->
                            K = list_to_binary(Key),
                            V = [list_to_integer(V) || [V,_] <- Vs, V =/= []],
                            TK = {proc_stat, K},
                            case ets:lookup(S#pnode.data, TK) of
                                [{_, PV}] ->
                                    ets:insert(S#pnode.data, {TK, V}),
                                    [{K, [Vc - Vp || {Vc, Vp} <- lists:zip(V, PV)]} | Acc];        
                                _Any -> 
                                    ets:insert(S#pnode.data, {TK, V}),
                                    Acc
                            end;
                    _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end, [], split_by_rn(D)).

split_by_rn(S) ->
    re:split(lists:concat(lists:map(fun(I) -> binary_to_list(I) end, S)), "\n").

%
% exec 
%

make_exec([], VNode, {A, CId, SName}, S) ->
    hipchat_transport:send(lists:concat([
            "Command series '", 
            binary_to_list(SName), 
            "' for hosts ",
            shepherd:host_to_string(S#pnode.host),
            "[", binary_to_list(VNode), "] is done! (awthanks)"
        ])),
    A ! {reply, [{op, <<"exec">>}, 
                 {id, CId}, 
                 {t, <<"done">>}]},
    S;

make_exec([Cmd | Cmds], VNode, {A, CId, _} = AC, S) ->
    Pid = self(),
    Command = case VNode of 
        <<"0">> -> Cmd;
        _ ->  
            CmdE = cmd_escape(Cmd),
            <<"vzctl exec ", VNode/binary, " \"", CmdE/binary, "\"">>
    end,
    A ! {reply, [{op, <<"exec">>}, 
                 {id, CId}, 
                 {t, <<"start">>},
                 {cmd, Command}]},
    ssh_agent:exec(S#pnode.agent, Command, {{postproc, fun(_Data) -> 
            Pid ! {exec, Cmds, VNode, AC}
        end}, {eachproc, fun(Data) -> 
            A ! {reply, [{op, <<"exec">>}, 
                         {id, CId}, 
                         {t, <<"console">>},
                         {v, Data}]}
        end}}),
    S.

cmd_escape(Cmd) ->
    re:replace(Cmd, "\"", "\\\\\"", [global, {return ,binary}]).
