-module(jabber_transport).
-behaivour(gen_server).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([send/2, set_status/1, join/2, leave/2, say/3]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Params) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).
init(Params) -> S = login(Params), {ok, S}.

send(To, Message) -> gen_server:call(?MODULE, {send, To, Message}).
set_status(Status) -> gen_server:call(?MODULE, {status, Status}).
join(R, N) -> gen_server:call(?MODULE, {join, R, N}).
say(R, N, M) -> gen_server:call(?MODULE, {say, R, N, M}).
leave(R, N) -> gen_server:call(?MODULE, {leave, R, N}).

%
% gen_server
%

handle_call({send, To, Msg}, _From, S) -> 
    send(S, To, Msg),
    {reply, ok, S};
handle_call({status, Status}, _From, S) -> 
    status(S, Status),
    {reply, ok, S};
handle_call({join, R, N}, _From, S) -> 
    join_room(S, R, N),
    {reply, ok, S};
handle_call({leave, R, N}, _From, S) -> 
    leave_room(S, R, N),
    {reply, ok, S};
handle_call({say, R, N, M}, _From, S) -> 
    say_to_room(S, R, N, M),
    {reply, ok, S};
handle_call(_Msg, _From, S) -> 
    {reply, ok, S}.

handle_cast(_Msg, S) -> 
    {noreply, S}.

handle_info(Msg, S) -> 
    case Msg of 
        #received_packet{packet_type=message, type_attr=Type} when Type =/= "error" ->
            handle_message(S, Msg);
        #received_packet{packet_type=presence} ->
            handle_presence(S, Msg); 
        _ ->
            handle_default(S, Msg)
    end,    
    {noreply, S}.

terminate(_Reason, S) -> exmpp_session:stop(S), ok.
code_change(_OldVsn, S, _Extra) -> {ok, S}.

%
% local
%

login(Params) ->
    JID = exmpp_jid:make(
            proplists:get_value(user, Params, "bot"), 
            proplists:get_value(server, Params, "localhost"), 
            random),
    Password = proplists:get_value(password, Params, "123456"),

    S = exmpp_session:start(),
    exmpp_session:auth_basic_digest(S, JID, Password),
    {ok, _StreamId} = exmpp_session:connect_TCP(S, proplists:get_value(server, Params, "localhost"), 5222),

    try exmpp_session:login(S)
    catch
    throw:{auth_error, 'not-authorized'} ->
            ?INFO("Register"),
            exmpp_session:register_account(S, Password),
            exmpp_session:login(S)
        end,
    status(S, "Here I am!"),
    S.

send(S, To, Text) -> 
    ToJID = exmpp_jid:parse(To),
    P = case is_list(Text) of
        true ->         
            exmpp_stanza:set_recipient(exmpp_message:chat(list_to_binary(Text)), ToJID);        
        _ -> 
            exmpp_stanza:set_recipient(exmpp_message:chat(Text), ToJID)
    end,
    exmpp_session:send_packet(S, P).

status(S, Val) ->
    exmpp_session:send_packet(S, exmpp_presence:set_status(exmpp_presence:available(), Val)). 

handle_message(_S, P) ->
    From = exmpp_xml:get_attribute(P#received_packet.raw_packet, <<"from">>, <<"me@localhost">>),
    case exmpp_message:get_body(P#received_packet.raw_packet) of
        undefined -> ok;
        M -> process(From, M)
    end.

process(From, Msg) ->
    ?INFO("Got message ~p : ~p", [From, Msg]),
    ?ASYNC(jabber_transport:send(From, "No operations. Fcuk you (just in case).")).

handle_presence(S, P) ->
    case exmpp_jid:make(_From = P#received_packet.from) of
    JID ->
        case _Type = P#received_packet.type_attr of
        "available" ->
            ok;
        "unavailable" ->
            ok;
        "subscribe" ->
            presence(S, JID, exmpp_presence:subscribed()),
            presence(S, JID, exmpp_presence:subscribe());
        "subscribed" ->
            presence(S, JID, exmpp_presence:subscribed()),
            presence(S, JID, exmpp_presence:subscribe())
        end
    end.

handle_default(_S, P) ->
    ?DBG("Got ~p", [P]),
    ok.

presence(S, R, Pr) ->
    P = exmpp_stanza:set_recipient(Pr, R),
    exmpp_session:send_packet(S, P).

join_room(S, Room, Nick) ->
    exmpp_session:send_packet(S, exmpp_stanza:set_recipient(exmpp_presence:available(), Room ++ "/" ++ Nick)).

leave_room(S, Room, Nick) ->
    exmpp_session:send_packet(S, exmpp_stanza:set_recipient(exmpp_presence:unavailable(), Room ++ "/" ++ Nick)).

say_to_room(S, Room, _Nick, Text) ->
    ToJID = exmpp_jid:parse(Room),
    P = case is_list(Text) of
        true ->         
            exmpp_stanza:set_recipient(exmpp_message:groupchat(list_to_binary(Text)), ToJID);        
        _ -> 
            exmpp_stanza:set_recipient(exmpp_message:groupchat(Text), ToJID)
    end,
    exmpp_session:send_packet(S, P).
