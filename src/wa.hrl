%
% Common project options
%

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).
-define(ASYNC(F), proc_lib:spawn(fun() -> F end)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 50, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 50, Type, [I]}).
-define(CHILD(Id, I, Type, Param), {Id, {I, start_link, Param}, permanent, 50, Type, [I]}).

-define(BIN2INT(N), list_to_integer(binary_to_list(N))).

%
% Configuration
%

-define(CONFIG(Key, Default), wa_app:config(Key, Default)).
-define(PV(Key, Set), proplists:get_value(Key, Set)).
-define(PV(Key, Set, Default), proplists:get_value(Key, Set, Default)).

%
% Pub/Sub
%

-define(ME(Reg), gproc:reg({n, l, Reg})).
-define(LOOKUP(Reg), gproc:lookup_pid({n, l, Reg})).
-define(LOOKUPS(Reg), gproc:lookup_pids({n, l, Reg})).
-define(PUB(Event, Msg), pubsub:pub(Event, Msg)).
-define(SUB(Event), pubsub:sub(Event)).
-define(UNSUB(Event), pubsub:unsub(Event)).
-define(LOOKUP_SUB(Reg), gproc:lookup_pids({p, l, Reg})).

%
% WS
%

-define(TOKEN_LEN, 32).
-record(wsh, {
        sid = <<"">>,
        peers = sets:new()
    }).

%
% Mail
%

-define(TEMPLATES, [
        {"report", template_report},
        {"notify", template_notify}
    ]).
-record(mails, {
        relay = [],
        from = "",
        last_mails = []
    }).

-define(RECONNECT_TIMEOUT, 5 * 1000).
-define(MAIL_RESEND_TIMEOUT, 5 * 1000).
-define(MAX_FAIL_COUNT, 16).

%
% SSH agent
%

-record(ssha, {
        pnode = 0,
        con = undefined,
        channels = [],
        params = []
    }).

-define(DEFAULT_SSH_HOST, <<"127.0.0.1">>).
-define(DEFAULT_SSH_USER, "root").
-define(DEFAULT_SSH_PASSWORD, "").
-define(DEFAULT_SSH_PORT, 22).

-define(SSH_CONNECT_TIMEOUT, 10000).
-define(SSH_CHANNEL_TIMEOUT, 10000).
-define(SSH_COMMAND_TIMEOUT, 10000).

-define(SSH_TERM_WIDTH, 1000).
-define(SSH_TERM_HEIGHT, 1000).

-define(SSH_PACKET_SIZE, 32768).
-define(SSH_WINDOW_SIZE, 1024 * ?SSH_PACKET_SIZE).

%
% Pnodes
%

-record(shepherd, {}).
-record(pnode, {
        id = 0,
        host = ?DEFAULT_SSH_HOST, 
        port = ?DEFAULT_SSH_PORT,
        user = ?DEFAULT_SSH_USER,
        password = ?DEFAULT_SSH_PASSWORD,
        state = undefined,
        agent = undefined,
        data = undefined,
        n = 0
    }).

-define(CHECK_TIMESLICE, 1000).

%
% Incoming data check
%

-define(RE_ID, "^[0-9]*$").
-define(RE_IP, "^[0-9a-f\\.:]*$").

%
% Monitoring
%

-define(DATE2TS(DateTime), calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200).

-record(monitor, {
        params = [],
        states = [],
        ts = 0
    }).

-record(nginx, {
        params = [], 
        re = undefined
    }).

-define(NGINX_STATUS, "^Active connections: ([0-9]*) \nserver accepts handled requests\n ([0-9]*) ([0-9]*) ([0-9]*) \nReading: ([0-9]*) Writing: ([0-9]*) Waiting: ([0-9]*)").
-define(NGINX_PARAMS,   [ active, accepts, handled, requests, reading, writing, waiting]).
-define(NGINX_STRATEGY, [{active,   as_is}, 
                         {accepts,  as_is}, 
                         {handled,  mon_up},
                         {requests, mon_up},
                         {reading,  as_is},
                         {writing,  as_is} , 
                         {waiting,  as_is}]).
-define(RAW_STEPS, 100).
% active = reading + writing + waiting
