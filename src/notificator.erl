-module(notificator).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(Params) -> ?SUB(alerts), {ok, Params}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({alert, high, M}, State) ->  N = ?PV(notify, State), notify(N, M), {noreply, State};
handle_info({alert, _, M}, State) -> [N1|_] = ?PV(notify, State), notify([N1], M), {noreply, State};

handle_info(Info, State) -> 
    ?INFO("Unknown message ~p", [Info]),
    {noreply, State}.


terminate(_Reason, _State) -> 
    ok.


code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%
% local
%

notify(L, M) -> [([Mod:send(T, M) || T <- Targets]) || {Mod, Targets} <- L].
