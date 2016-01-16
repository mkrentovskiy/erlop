-module(sms_transport).
-behaviour(gen_server).

-export([send/2]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

send(To, Message) ->
    gen_server:cast(?MODULE, {send, To, Message}).

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(Params) -> 
    ?AFTER(0, check_balance),
    {ok, Params}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast({send, To, Message}, State) ->
    case req([{"action", "post_sms"}, {"target", To}, {"message", Message}], State) of
        {ok, Data} ->
            ?INFO("Sended with reply ~p", [Data]), 
            {noreply, State};
        Any ->
            ?ERROR("Got ~p on sending ~p to ~p", [Any, Message, To]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(check_balance, State) ->
    Int = case req([{"action", "balance"}], State) of
        {ok, D} -> 
            case re:run(D, "<AGT_BALANCE>([0-9\\.]*)</AGT_BALANCE>", [{capture, all, list}]) of
                {match, [_, SV]} -> 
                    {V, _} = string:to_float(SV),
                    case V < ?PV(balance_barier, State) of
                        true -> 
                            ?INFO("Balance ~p is less than barier.", [V]),
                            [?ASYNC(sms_transport:send(T, lists:concat(["Current SMS balance = ", SV]))) 
                                || T <- ?PV(balance_notify, State)],
                            ?PV(balance_check_long_period, State);
                        false -> 
                            ?PV(balance_check_period, State)
                    end;
                PR ->
                    ?ERROR("Can't parse balance result ~p -> ~p", [D, PR]),
                    ?PV(balance_check_period, State)
            end;
        Any ->
            ?ERROR("Can't check balance"),
            ?PV(balance_check_period, State)
    end,
    ?AFTER(Int, check_balance),
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

req(R, S) ->
    H = [{"Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"}],
    NR = R ++ [{"user", ?PV(login, S)}, {"pass", ?PV(password, S)}],
    case ibrowse:send_req(?PV(url, S), H, post, pl_to_url_encoded(NR, "")) of
        {ok, _, RH, D} -> 
            case proplists:get_value("Content-Encoding", RH) of
                "gzip" -> {ok, zlib:gunzip(D)};
                _ -> {ok, D}
            end;    
        Any -> {error, Any}
    end.

pl_to_url_encoded([], R) -> R;
pl_to_url_encoded([{K, V} | Tail], "") -> 
    pl_to_url_encoded(Tail, lists:concat([ibrowse_lib:url_encode(K), "=", ibrowse_lib:url_encode(V)]));
pl_to_url_encoded([{K, V} | Tail], R) -> 
    pl_to_url_encoded(Tail, lists:concat([R, "&", ibrowse_lib:url_encode(K), "=", ibrowse_lib:url_encode(V)])).
