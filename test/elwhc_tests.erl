-module(elwhc_tests).

-include("elwhc_private.hrl").
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
    ok.

default_port_test() ->

    ?assertEqual(80, elwhc:default_port(http)),
    ?assertEqual(443, elwhc:default_port(https)),

    ok.

build_options_test() ->

    Opts = [
          {connect_timeout_ms,1}
        , {request_timeout_ms,2}
        , {keepalive_ms,3}
        , {keepalive,true}
        , {tcp_connect_options, [tcp]}
        , {ssl_options, [ssl]}
        , {max_sessions,4}
    ],

    Expected = #elwhc_opts {
          connect_timeout_ms = 1
        , request_timeout_ms = 2
        , keepalive_ms = 3
        , keepalive = true
        , tcp_connect_options = [tcp]
        , ssl_options = [ssl]
        , max_sessions = 4
    },

    ?assertEqual(Expected, elwhc:build_options(Opts, #elwhc_opts{})),

    ok.


teardown_test() ->
    ok.

%EOF
