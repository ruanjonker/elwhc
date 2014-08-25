-module(elwhc_tests).

-include("elwhc_private.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(stop_deps, elwhc_test_helper:stop_app_deps()).

setup_test() ->
    error_logger:tty(false),
    ?stop_deps,
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
        , {max_requests_per_session,5}
    ],

    Expected = #elwhc_opts {
          connect_timeout_ms = 1
        , request_timeout_ms = 2
        , keepalive_ms = 3
        , keepalive = true
        , tcp_connect_options = [tcp]
        , ssl_options = [ssl]
        , max_sessions = 4
        , max_requests_per_session = 5
    },

    ?assertEqual(Expected, elwhc:build_options(Opts, #elwhc_opts{})),

    ok.

start_deps_test() ->

    ?assertEqual([asn1, crypto, public_key,ssl], ?ELWHC_START_DEPS),

    [
        begin
            ?assertEqual({error, {not_started,Dep}}, application:start(elwhc)),
            ?assertEqual(ok, application:start(Dep)) 
        end

    || Dep <- ?ELWHC_START_DEPS],

    ?assertEqual(ok, application:start(elwhc)),

    ?assertEqual(ok, application:stop(elwhc)),

    ?stop_deps,

    ok.

start_dev_test() ->

    ?assertEqual(ok, elwhc_app:start_dev()),

    ?assertEqual(ok, application:stop(elwhc)),

    ?stop_deps,

    ok.

teardown_test() ->
    ok.

%EOF
