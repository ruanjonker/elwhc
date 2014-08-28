-module(elwhc_handler_tests).

-include("elwhc_private.hrl").
-include_lib("eunit/include/eunit.hrl").


setup_test() ->

    ?assertEqual(ok, elwhc_test_helper:stop_app_deps()),
 
    ?assertEqual(ok, elwhc_test_helper:start_app_deps()), 
    
    ?assertEqual(ok, application:start(elwhc)),


    ok.

merge_headers_test() ->

    E0 = lists:sort([{"Host", "myhost.com"}, {"User-Agent", "ELWHC/1.0"}]),
    ?assertEqual(E0, lists:sort(elwhc_handler:merge_headers("myhost.com", false, undefined, []))),

    E1 = lists:sort([{"host", "user's host"}, {"User-Agent", "ELWHC/1.0"}]),
    ?assertEqual(E1, lists:sort(elwhc_handler:merge_headers("myhost.com", false, undefined, [{"host", "user's host"}]))),
     
    E2 = lists:sort([{"Connection", "keep-alive"},{"Host", "myhost.com"}, {"User-Agent", "ELWHC/1.0"}]),
    ?assertEqual(E2, lists:sort(elwhc_handler:merge_headers("myhost.com", true, undefined, []))),

    E3 = lists:sort([{"Connection", "keep-alive"},{"host", "user's host"}, {"User-Agent", "ELWHC/1.0"}]),
    ?assertEqual(E3, lists:sort(elwhc_handler:merge_headers("myhost.com", true, undefined, [{"host", "user's host"}, {"connection", "close"}]))),

    E4 = lists:sort([{"Connection", "keep-alive"},{"host", "user's host"}, {"User-Agent", "ELWHC/1.0"}, {"Content-Length", "10"}]),
    ?assertEqual(E4, lists:sort(elwhc_handler:merge_headers("myhost.com", true, 10, [{"host", "user's host"}, {"connection", "close"}, {"Content-Length", "19999"}]))),

    E5 = lists:sort([{"Connection", "keep-alive"},{"host", "user's host"}, {"user-agent", "user's ua"}]),
    ?assertEqual(E5, lists:sort(elwhc_handler:merge_headers("myhost.com", true, undefined, [{"host", "user's host"}, {"user-agent", "user's ua"}]))),

    ok.

basic_server_stream_from_fun_test() ->

    TestPid = self(),

    {StreamPid, SRef} = spawn_monitor(fun() -> stream_loop(0, 5) end),
    
    StreamFun = fun() ->
        DRef = make_ref(),
        StreamPid ! {more, DRef, self()},
        receive {DRef, Data} -> Data end
    end,

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,34}, 12345) end),

    rot(listening),

%%%%%%%%First request
    spawn(fun() -> Res = elwhc:request('POST', "http://127.0.0.34:12345/pa/th?a=b#12345", <<>>, [], [{stream_from, StreamFun}]), TestPid ! Res end),

    rot(accepted),
    %Cheating a bit here - waiting for all tcp packets to be sent , then do receive,,,,
    rot ({'DOWN', SRef, process, StreamPid, normal}),
    ServerPid ! loop,

    rot({payload, 
        {ok, <<"POST /pa/th?a=b#12345 HTTP/1.0\r\nHost:127.0.0.34:12345\r\nUser-Agent:ELWHC/1.0\r\n\r\n1\r\n0\r\n1\r\n1\r\n1\r\n2\r\n1\r\n3\r\n1\r\n4\r\n0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.0 200 OK\r\nServer:bla\r\nTransfer-Encoding:bla,chunked,identity\r\n\r\n2;ext=ext-value\r\n12\r\n5\r\n34567\r\n0\r\nMore-Headers:Header-Value\r\n\r\n">>},

    rot({ok, 200, [{"more-headers", "More-Headers","Header-Value"}, {"server", "Server", "bla"}, {"transfer-encoding", "Transfer-Encoding", "bla,identity"}], <<"1234567">>}),

    ServerPid ! close,

    ServerPid ! die,

    rot({'DOWN', Ref, process, ServerPid, normal}),

    ok.

basic_server_transfer_chunked_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,35}, 12345) end),

    rot(listening),

%%%%%%%%First request
    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.35:12345/pa/th?a=b#12345", <<>>, [], [{keepalive, true}]), TestPid ! Res end),

    rot(accepted),

    ServerPid ! loop,    

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.1\r\nHost:127.0.0.35:12345\r\nConnection:keep-alive\r\nUser-Agent:ELWHC/1.0\r\nContent-Length:0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\nTransfer-Encoding:bla,chunked,identity\r\n\r\n2;ext=ext-value\r\n12\r\n5\r\n34567\r\n0\r\nMore-Headers:Header-Value\r\n\r\n">>},

    rot({ok, 200, [{"more-headers", "More-Headers","Header-Value"}, {"server", "Server", "bla"}, {"transfer-encoding", "Transfer-Encoding", "bla,identity"}], <<"1234567">>}),


    ServerPid ! loop,

%%%%%%%%% Second request
    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.35:12345/pa/th?a=b2#12345", <<>>, [], [{keepalive, true}]), TestPid ! Res end),

    rot({payload, 
        {ok, <<"GET /pa/th?a=b2#12345 HTTP/1.1\r\nHost:127.0.0.35:12345\r\nConnection:keep-alive\r\nUser-Agent:ELWHC/1.0\r\nContent-Length:0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\nContent-Length:5\r\nX-Test:bla\r\n\r\nHello">>},

    ServerPid ! close,

    rot({ok, 200, [{"server", "Server", "bla"}, {"content-length", "Content-Length", "5"}, {"x-test", "X-Test", "bla"}], <<"Hello">>}),

    ServerPid ! die,

    rot({'DOWN', Ref, process, ServerPid, normal}),

    ok.

basic_server_keepalive_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,36}, 12345) end),

    rot(listening),


%%%%%%%%First request
    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.36:12345/pa/th?a=b#12345", <<>>, [], [{keepalive, true}]), TestPid ! Res end),

    rot(accepted),

    ServerPid ! loop,

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.1\r\nHost:127.0.0.36:12345\r\nConnection:keep-alive\r\nUser-Agent:ELWHC/1.0\r\nContent-Length:0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\nContent-Length:5\r\nX-Test:bla\r\n\r\nHello">>},

    rot({ok, 200, [{"server", "Server", "bla"}, {"content-length", "Content-Length", "5"}, {"x-test", "X-Test", "bla"}], <<"Hello">>}),

    ServerPid ! loop,


%%%%%%%%% Second request
    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.36:12345/pa/th?a=b2#12345", <<>>, [], [{keepalive, true}]), TestPid ! Res end),

    rot({payload, 
        {ok, <<"GET /pa/th?a=b2#12345 HTTP/1.1\r\nHost:127.0.0.36:12345\r\nConnection:keep-alive\r\nUser-Agent:ELWHC/1.0\r\nContent-Length:0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\nContent-Length:5\r\nX-Test:bla\r\n\r\nHello">>},

    ServerPid ! close,

    rot({ok, 200, [{"server", "Server", "bla"}, {"content-length", "Content-Length", "5"}, {"x-test", "X-Test", "bla"}], <<"Hello">>}),

    ServerPid ! die,

    rot({'DOWN', Ref, process, ServerPid, normal}),

    ok.



basic_server_max_requests_per_session_reached_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,39}, 12345) end),

    rot(listening),


%%%%%%%%First request
    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.39:12345/pa/th?a=b#12345", <<>>, [], [{keepalive, true}, {max_requests_per_session, 1}]), TestPid ! Res end),

    rot(accepted),
    ServerPid ! loop,

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.1\r\nHost:127.0.0.39:12345\r\nConnection:keep-alive\r\nUser-Agent:ELWHC/1.0\r\nContent-Length:0\r\n\r\n">>}}),

    [Session] = [ Session  || [#elwhc_session{host = "127.0.0.39", port = 12345} = Session] <- elwhc_sessions:list()],

    ?assertMatch(#elwhc_session{host = "127.0.0.39", port = 12345}, Session),

    HndlrRef = monitor(process, Session#elwhc_session.pid),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\nContent-Length:5\r\nX-Test:bla\r\n\r\nHello">>},

    receive

        {ok, 200, [{"server", "Server", "bla"}, {"content-length", "Content-Length", "5"}, {"x-test", "X-Test", "bla"}], <<"Hello">>} ->

        rot({'DOWN', HndlrRef, process, Session#elwhc_session.pid, normal})

    end,

    exit(ServerPid, kill),

    rot({'DOWN', Ref, process, ServerPid, killed}),

    ok.


basic_server_content_length_close_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,37}, 12345) end),

    rot(listening),

    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.37:12345/pa/th?a=b#12345", <<>>, [{"User-Agent", "MyApp"}], []), TestPid ! Res end),

    rot(accepted),
    ServerPid ! loop,

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.0\r\nUser-Agent:MyApp\r\nHost:127.0.0.37:12345\r\nContent-Length:0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\nContent-Length:5\r\nX-Test:bla\r\n\r\nHello">>},

    ServerPid ! close,

    rot({ok, 200, [{"server", "Server", "bla"}, {"content-length", "Content-Length", "5"}, {"x-test", "X-Test", "bla"}], <<"Hello">>}),

    ServerPid ! die,

    rot({'DOWN', Ref, process, ServerPid, normal}),

    ok.

basic_server_connection_close_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,38}, 12345) end),

    rot(listening),

    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.38:12345/pa/th?a=b#12345", <<>>, [], []), TestPid ! Res end),

    rot(accepted),
    ServerPid ! loop,

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.0\r\nHost:127.0.0.38:12345\r\nUser-Agent:ELWHC/1.0\r\nContent-Length:0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\n\r\nHello">>},

    ServerPid ! close,

    rot({ok, 200, [{"server", "Server", "bla"}], <<"Hello">>}),
    
    ServerPid ! die,

    rot({'DOWN', Ref, process, ServerPid, normal}),

    ok.

receive_timeout_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,38}, 12345) end),

    rot(listening),

    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.38:12345/pa/th?a=b#12345", <<>>, [], [{request_timeout_ms, 100}]), TestPid ! Res end),

    rot(accepted),
    ServerPid ! loop,

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.0\r\nHost:127.0.0.38:12345\r\nUser-Agent:ELWHC/1.0\r\nContent-Length:0\r\n\r\n">>}}),

    timer:sleep(200),

    rot({error, timeout}),
 
    exit(ServerPid, kill),   

    rot({'DOWN', Ref, process, ServerPid, killed}),

    ok.

update_ttg_test() ->

%%%%%%%%%%
    I0 = #elwhc_request{request_ttg_ms = 1000, request_ttg_t0 = {0,0,0}},
    E0 = I0#elwhc_request{request_ttg_ms = 0, request_ttg_t0 = {0,1,0}},
    ?assertEqual(E0, elwhc_handler:update_ttg(I0, {0,1,0})),

%%%%%%%%%%
    I1 = #elwhc_request{request_ttg_ms = 1000, request_ttg_t0 = {0,0,0}},
    E1 = I1#elwhc_request{request_ttg_ms = 0, request_ttg_t0 = {0,1,1}},
    ?assertEqual(E1, elwhc_handler:update_ttg(I1, {0,1,1})),

%%%%%%%%%%
    I2 = #elwhc_request{request_ttg_ms = 2000, request_ttg_t0 = {0,0,0}},
    E2 = I2#elwhc_request{request_ttg_ms = 1500, request_ttg_t0 = {0,0,500000}},
    ?assertEqual(E2, elwhc_handler:update_ttg(I2, {0,0,500000})),


    ok.

build_headers_test() ->

    ?assertEqual([], elwhc_handler:build_headers([], [])),

    ?assertEqual(whatever, elwhc_handler:build_headers([], whatever)), %hmmm, do we need a guard for this ?

    ?assertEqual(["a", $:, "b", $\r, $\n], elwhc_handler:build_headers([{"a", "b"}], [])),

    ?assertEqual(["c", $:, "d", $\r, $\n, "a", $:, "b", $\r, $\n], elwhc_handler:build_headers([{"a", "b"}, {"c", "d"}], [])),

    ?assertEqual(["c ", $:, " d", $\r, $\n, "a a", $:, " b", $\r, $\n], elwhc_handler:build_headers([{"a a", " b"}, {"c ", " d"}], [])),

    ok.

teardown_test() ->

    ?assertEqual(ok, application:stop(elwhc)),

    ?assertEqual(ok, elwhc_test_helper:stop_app_deps()),
 
    ok.

basic_server(TestPid, If, Port) ->

    {ok, LS} = gen_tcp:listen(Port, [{ip, If}, {reuseaddr, true}]),

    TestPid ! listening,

    {ok, AS} = gen_tcp:accept(LS),

    ok = inet:setopts(AS, [binary, {packet, raw}, {active, false}]),

    TestPid ! accepted,

    receive loop -> ok end,

    basic_server_rx_loop(TestPid, LS, AS).
    

basic_server_rx_loop(TestPid, LS, AS) ->

    RxRes = gen_tcp:recv(AS, 0),

    TestPid ! {payload, RxRes},

    receive {rsp_payload, RspPayload} ->

        ?assertEqual(ok, gen_tcp:send(AS, RspPayload)),

        receive close ->
            ?assertEqual(ok, gen_tcp:close(AS)),

            ?assertEqual(ok, gen_tcp:close(LS)),

            receive die -> ok end;

        loop ->

            basic_server_rx_loop(TestPid, LS, AS)

        end;

    loop -> 
        basic_server_rx_loop(TestPid, LS, AS)

    end.


rot(Expected) ->
%    ?debugFmt("~nROT: ~p~n", [Expected]),
    receive Expected ->
%    ?debugFmt("~nROTGOT: ~p~n", [Expected]),
        ok;
    Other ->
        ?debugFmt("~nROT E: ~p~n", [Expected]),
        ?debugFmt("~nROT T: ~p~n", [Other]),
        throw(Other)
    end.

stream_loop(Count, MaxChunks) ->
    receive 
    {more, Ref, Pid} ->
        if (Count >= MaxChunks) ->
            Pid ! {Ref, <<>>};
        true ->
            Pid ! {Ref, list_to_binary(integer_to_list(Count))},
            stream_loop(Count+1, MaxChunks)
        end
    end.



%EOF
