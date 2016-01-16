-module(hipchat_transport).
-behaviour(gen_server).

-export([send/1]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

send(Message) ->
    gen_server:cast(?MODULE, {send, Message}).

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(Params) -> 
    {ok, Params}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast({send, Message}, State) ->
    URL = lists:concat([
            "https://api.hipchat.com/v2/room/", 
            ?PV(room, State), 
            "/notification"
        ]),
    H = [{"Content-Type", "application/json"},
         {"Authorization", "Bearer " ++ ?PV(token, State)}],
    M = case is_list(Message) of true -> list_to_binary(Message); false -> Message end,
    Body = jsonx:encode([
            {color, ?PV(color, State)}, 
            {message, M},
            {notify, false},
            {message_format, <<"text">>}
        ]),
    case ibrowse:send_req(URL, H, post, Body) of
        {ok, Status, RH, _D} -> 
            ?INFO("Got reply ~p with headers ~p", [Status, RH]);    
        Any -> 
            ?ERROR("Got ~p on sending ~p", [Any, Message])       
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

        
handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, _State) -> 
    ok.


code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

