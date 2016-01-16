-module(nginx_monitor).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

init_state(Params) -> 
    {ok, RE} = re:compile(?NGINX_STATUS),
    #nginx{ params = Params, re = RE }.

check(State) ->
    Nodes = persist:get_pnodes(pgdb),
    Suffix = ?PV(suffix, State#nginx.params), 
    lists:map(fun({IdB, IP, _, _, _}) ->
            Id = binary_to_integer(IdB),
            case ibrowse:send_req(lists:concat(["http://", binary_to_list(IP), Suffix]), [], get) of
                {ok, "200", Headers, Body} -> ?ASYNC(extact_stat(Id, Headers, Body, State#nginx.re));
                Any -> ?ERROR("Got reply for nginx on ~p - ~p", [IP, Any])
            end
        end, Nodes),
    {?PV(period, State#nginx.params), State}.

extact_stat(Id, Headers, Body, RE) ->
    case re:run(Body, RE, [{capture, all, list}]) of
        {match, [_|List]} ->
            P = maps:from_list(lists:zip(?NGINX_PARAMS, [list_to_integer(I) || I <- List])),
            Date = case ?PV("Date", Headers, undefined) of
                undefined -> calendar:local_time();
                DateS -> qdate:to_date(DateS)
            end,
            ?PUB({agg, Id}, {Date, ?MODULE, P});
        Any -> 
            ?WARNING("Can't parse nginx output ~p - ~p", [Body, Any])
    end.

start_link(Params) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).
init(Params) -> 
    ?AFTER(0, monitor), 
    {ok, init_state(Params)}.

handle_call(_Msg, _From, State) ->  {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(monitor, State) ->
    {Period, NewState} = check(State),
    ?AFTER(Period, monitor),    
    {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
