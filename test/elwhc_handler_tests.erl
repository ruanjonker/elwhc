-module(elwhc_handler_tests).

-include("elwhc_private.hrl").
-include_lib("eunit/include/eunit.hrl").


setup_test() ->

    ?assertEqual(ok, elwhc_test_helper:stop_app_deps()),
 
    ?assertEqual(ok, elwhc_test_helper:start_app_deps()), 
    
    ?assertEqual(ok, application:start(elwhc)),


    ok.



basic_server_content_length_close_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,36}, 12345) end),

    rot(listening),

    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.36:12345/pa/th?a=b#12345", <<>>, [], []), TestPid ! Res end),

    rot(accepted),

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.0\r\nHost:127.0.0.36:12345\r\nContent-Length:0\r\nUser-Agent:ELWHC/1.0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\nContent-Length:5\r\nX-Test:bla\r\n\r\nHello">>},

    ServerPid ! close,

    rot({ok, 200, [{"Server", "bla"}, {"Content-Length", "5"}, {"X-Test", "bla"}], <<"Hello">>}),

    ServerPid ! die,

    rot({'DOWN', Ref, process, ServerPid, normal}),

    ok.

basic_server_connection_close_test() ->

    TestPid = self(),

    {ServerPid, Ref} = spawn_monitor(fun() -> basic_server(TestPid, {127,0,0,36}, 12345) end),

    rot(listening),

    spawn(fun() -> Res = elwhc:request('GET', "http://127.0.0.36:12345/pa/th?a=b#12345", <<>>, [], []), TestPid ! Res end),

    rot(accepted),

    rot({payload, 
        {ok, <<"GET /pa/th?a=b#12345 HTTP/1.0\r\nHost:127.0.0.36:12345\r\nContent-Length:0\r\nUser-Agent:ELWHC/1.0\r\n\r\n">>}}),

    ServerPid ! {rsp_payload, <<"HTTP/1.1 200 OK\r\nServer:bla\r\n\r\nHello">>},

    ServerPid ! close,

    rot({ok, 200, [{"Server", "bla"}], <<"Hello">>}),
    
    ServerPid ! die,

    rot({'DOWN', Ref, process, ServerPid, normal}),

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

    RxRes = gen_tcp:recv(AS, 0),

    TestPid ! {payload, RxRes},

    receive {rsp_payload, RspPayload} ->

        ?assertEqual(ok, gen_tcp:send(AS, RspPayload)),

        receive close ->
            ?assertEqual(ok, gen_tcp:close(AS)),

            ?assertEqual(ok, gen_tcp:close(LS))

        end,

        receive die -> ok end

    end.


rot(Expected) ->
%    ?debugFmt("~nROT: ~p~n", [Expected]),
    receive Expected ->
%    ?debugFmt("~nROTGOT: ~p~n", [Expected]),
        ok;
    Other ->
        throw(Other)
    end.


%EOF
