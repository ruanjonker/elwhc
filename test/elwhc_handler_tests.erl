-module(elwhc_handler_tests).

-include("elwhc_private.hrl").
-include_lib("eunit/include/eunit.hrl").


setup_test() ->

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

    %Just make sure that header fields and values are urlencoded ...
    ?assertEqual(["c%20", $:, "%20d", $\r, $\n, "a%20a%0A", $:, "%20b", $\r, $\n], elwhc_handler:build_headers([{"a a\n", " b"}, {"c ", " d"}], [])),

    ok.

teardown_test() ->

    ok.


%EOF
