-module(shepherd).
-behaviour(gen_server).

-export([add_pnode/2, drop_pnode/2]).
-export([id_from_db/1, host_to_db/1, host_to_string/1, host_to_json/1]). 
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

add_pnode(IP, A) -> gen_server:cast(?MODULE, {add, IP, A}).
drop_pnode(Id, A) -> gen_server:cast(?MODULE, {drop, Id, A}).
update_pnode(Id, A, Data) -> gen_server:cast(?MODULE, {update, Id, A, Data}).
%
% external
%
 
start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(_Params) ->
    ?AFTER(0, start_pnodes),
    {ok, #shepherd{}}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.

handle_cast({add, IP, A}, State) ->
    ?INFO("Add node with IP ~p", [IP]),
    case persist:add_pnode(pgdb, host_to_db(IP)) of
        0 -> A ! {reply, [{op, add_pnode}, {result, error}]};
        Id -> 
            ?INFO("Start pnode ~p with id ~p", [IP, Id]),
            start_pnode(Id, IP),
            A ! {reply, [{op, add_pnode}, {result, ok}]}
    end,
    {noreply, State};

handle_cast({update, Id, _A, Data}, State) when is_integer(Id) ->
    CId = child_id(Id),
    ?INFO("Update node with cid ~p", [CId]),
    persist:update_pnode(pgdb, Id, Data),
    ?PUB(clients, {reply, [{op, update_pnode}, {id, Id}, {d, Data}]}),
    {noreply, State};

handle_cast({drop, Id, _A}, State) when is_integer(Id) ->
    CId = child_id(Id),
    ?INFO("Drop node with cid ~p", [CId]),
    persist:drop_pnode(pgdb, Id),
    supervisor:terminate_child(wa_sup, CId), 
    ?PUB(clients, {reply, [{op, drop_pnode}, {id, Id}]}),
    {noreply, State};

handle_cast(Msg, State) ->
    ?WARNING("Shepherd in panic ~p", [Msg]), 
    {noreply, State}.


handle_info(start_pnodes, State) ->
    Nodes = persist:get_pnodes(pgdb),
    lists:map(fun({Id, IP, _, _}) -> start_pnode(id_from_db(Id), host_from_db(IP)) end, Nodes),
    {noreply, State};

handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, _State) -> 
    ok.


code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%
% local
%

start_pnode(Id, IP) ->
    CId = child_id(Id),
    ?INFO("Start pnode with id ~p", [CId]),
    case supervisor:start_child(wa_sup, ?CHILD(CId, pnode, worker, [{Id, IP}])) of
        {ok, ChildId} -> ?INFO("Started child with id ~p", [ChildId]);
        {ok, ChildId, Info} -> ?INFO("Started child with id ~p and info ~p", [ChildId, Info]);
        Any -> ?ERROR("Start child error ~p", [Any])
    end.

child_id(Id) -> {pnode, Id}.

%
% Type casts
%

id_from_db(Id) when is_binary(Id) -> id_from_db(binary_to_list(Id));
id_from_db(Id) when is_list(Id) -> list_to_integer(Id);
id_from_db(Id) when is_integer(Id) -> Id.

host_from_db(H) when is_binary(H) -> H;
host_from_db(H) -> list_to_binary(host_to_string(H)).

host_to_db(H) when is_binary(H) -> host_to_db(binary_to_list(H));
host_to_db(H) when is_list(H) -> 
    case inet_parse:address(H) of
        {ok, NH} -> NH;
        Any -> ?ERROR("Error in parsing IP ~p - ~p", [H, Any]), {0, 0, 0, 0}
    end;
host_to_db(H) when is_tuple(H) -> H.

host_to_json(H) when is_binary(H) -> H;
host_to_json(H) when is_tuple(H) -> list_to_binary(inet_parse:ntoa(H));
host_to_json(H) when is_list(H) -> list_to_binary(H).

host_to_string(H) when is_binary(H) -> binary_to_list(H);
host_to_string(H) when is_tuple(H) -> inet_parse:ntoa(H);
host_to_string(H) when is_list(H) -> H.
