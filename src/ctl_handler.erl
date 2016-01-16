-module(ctl_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_TransportName, Req, _Opts) ->
    SID = token(),
    ?PUB(clients, {add_peer, SID}),
    ?PUB(clients, {send_sid_to, self()}),
    [?SUB(E) || E <- [clients, {client, SID}, clogs]],
    {ok, Req, #wsh{ sid = SID }}.


websocket_handle({text, Msg}, Req, State) ->
    process(jsonx:decode(Msg, [{format, proplist}]), State),
    {ok, Req, State};

websocket_handle(Data, Req, State) ->
    ?DBG("Unknown data handled: ~p", [Data]),
    {ok, Req, State}.

websocket_info({add_peer, SID}, Req, State) ->
    update_peers(sets:add_element(SID, State#wsh.peers), Req, State);
websocket_info({drop_peer, SID}, Req, State) -> 
    ?AFTER(0, {reply, [
            {<<"op">>, <<"call">>},
            {<<"s">>, <<"drop">>},
            {<<"from">>, SID}
        ]}),
    update_peers(sets:del_element(SID, State#wsh.peers), Req, State);
websocket_info({send_sid_to, Pid}, Req, State) ->
    Pid ! {add_peer, State#wsh.sid},
    {ok, Req, State};

websocket_info({reply, Msg}, Req, State) -> 
    out(Msg, Req, State);
websocket_info({clogs, L}, Req, State) -> 
    out([{<<"op">>,<<"clogs">>}, 
         {<<"v">>, L}], 
         Req, State);

websocket_info(Info, Req, State) ->
    ?DBG("Unknown info handled: ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
    ?PUB(clients, {drop_peer, State#wsh.sid}),
    ok.

%
% processing incoming request
%

out(Msg, Req, State) ->
    case jsonx:encode(Msg) of
        Str when is_binary(Str) -> {reply, {text, Str}, Req, State};
        Err -> ?ERROR("Error encoding to JSON ~p in ~p", [Err, Msg]), {ok, Req, State} 
    end.

update_peers(Peers, Req, State) ->
    out([{<<"op">>,<<"peers">>},
         {<<"count">>, sets:size(Peers) + 1}], 
         Req, 
         State#wsh{ peers = Peers }).

token() -> list_to_binary(random_str(?TOKEN_LEN)).
random_str(Length) ->
    Alpha = "1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM",
    Size = length(Alpha),
    [lists:nth(crypto:rand_uniform(1, Size), Alpha) || _ <- lists:seq(1, Length)].

% pnodes
process([{<<"op">>,<<"info_pnodes">>}], _) -> ?PUB(pnodes, {info, self()});
process([{<<"op">>,<<"add_pnode">>},{<<"ip">>, IP}], _) -> shepherd:add_pnode(IP, self());
process([{<<"op">>,<<"drop_pnode">>},{<<"id">>, Id}], _) -> shepherd:drop_pnode(Id, self());

% scenarios
process([{<<"op">>,<<"scenarios">>}], _) -> 
    ?AFTER(0, {reply, [{op, <<"scenarios">>}, {ct, <<"list">>}, {v, persist:get_scenarios_list(pgdb)}]}); 
process([{<<"op">>,<<"scenario">>}, {<<"id">>, Id}], _) -> 
    ?AFTER(0, {reply, [{op, <<"scenarios">>}, {ct, <<"item">>}, {v, persist:get_scenario(pgdb, Id)}]}); 
process([{<<"op">>,<<"scenario_edit">>}, {<<"id">>, Id}], _) -> 
    ?AFTER(0, {reply, [{op, <<"scenarios">>}, {ct, <<"item_edit">>}, {v, persist:get_scenario(pgdb, Id)}]}); 
process([{<<"op">>,<<"scenario_update">>},
         {<<"obj">>,[{<<"id">>, Id}, 
                     {<<"name">>,Name}, 
                     {<<"params">>, Params},
                     {<<"commands">>, Commands}]}], _) ->
    persist:update_scenario(pgdb, Id, Name, Params, Commands),
    ?AFTER(0, {reply, [{op, <<"scenarios">>}, {ct, <<"update">>}]}),
    ?PUB(clients, {reply, [{op, <<"scenarios">>}, {ct, <<"list">>}, {v, persist:get_scenarios_list(pgdb)}]}); 
process([{<<"op">>,<<"scenario_drop">>}, {<<"id">>, Id}], _) ->
    persist:drop_scenario(pgdb, Id),
    ?PUB(clients, {reply, [{op, <<"scenarios">>}, {ct, <<"list">>}, {v, persist:get_scenarios_list(pgdb)}]}); 

process([{<<"op">>,<<"datasource">>},
         {<<"type">>, Type},
         {<<"id">>, CId},
         {<<"pnode">>, PNode}], _) -> ?PUB({pnode, shepherd:id_from_db(PNode)}, {datasource, self(), Type, <<"0">>, CId});
process([{<<"op">>,<<"datasource">>},
         {<<"type">>, Type},
         {<<"id">>, CId},
         {<<"pnode">>, PNode},
         {<<"vnode">>, VNode}], _) -> ?PUB({pnode, shepherd:id_from_db(PNode)}, {datasource, self(), Type, VNode, CId});
process([{<<"op">>,<<"exec">>},
         {<<"id">>, CId},
         {<<"scenario">>, Id},
         {<<"pnode">>, PNode},
         {<<"vnode">>, VNode},
         {<<"cmds">>, Cmds}], _) -> 
            persist:inc_scenatio_rate(pgdb, Id),
            SName = persist:get_scenario_title(pgdb, Id),
            ?PUB({pnode, shepherd:id_from_db(PNode)}, {exec, self(), Cmds, VNode, CId, SName});

% peers
process([{<<"op">>,<<"call">>}, {<<"to">>, <<"all">>}|_] = R, State) -> 
    M = R ++ [{<<"from">>, State#wsh.sid}],
    [?PUB({client, SID}, {reply, M}) || SID <- sets:to_list(State#wsh.peers)];
process([{<<"op">>,<<"call">>}, {<<"to">>, To}|_] = R, State) -> 
    ?PUB({client, To}, {reply, R ++ [{<<"from">>, State#wsh.sid}]});

% default
process(Msg, _) -> 
    ?DBG("Unknown message ~p", [Msg]), 
    ?AFTER(0, {reply, [{<<"op">>, <<"reload">>}]}),
    ok. 
