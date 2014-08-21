-module(elwhc_sessions_tests).

-include("elwhc_private.hrl").
-include_lib("eunit/include/eunit.hrl").

setup_test() ->

    ?assertMatch({ok, Pid} when is_pid(Pid), elwhc_sessions:start_link()),

    ok.

list_test() ->
    ?assertEqual([], elwhc_sessions:list()),
    ok.

select_test() ->

    Result =  elwhc_sessions:select(http, "www.somehost.com", 12345, 10),

    ?assertMatch({ok, Pid} when is_pid(Pid), Result),

    {ok, Pid} = Result,

    ?assertMatch([[#elwhc_session{scheme = http, host = "www.somehost.com", port = 12345, pid = Pid, status = in_use}]], elwhc_sessions:list()),

    elwhc_test_helper:kill(Pid, kill),

    ?assertEqual([], elwhc_sessions:list()),

    ok.

free_test() ->

    Result =  elwhc_sessions:select(http, "www.somehost.com", 12345, 10),

    ?assertMatch({ok, Pid} when is_pid(Pid), Result),

    {ok, Pid} = Result,

    ?assertMatch([[#elwhc_session{scheme = http, host = "www.somehost.com", port = 12345, pid = Pid, status = in_use}]], elwhc_sessions:list()),

    ?assertMatch(ok, elwhc_sessions:free(Pid)),

    ?assertMatch([[#elwhc_session{scheme = http, host = "www.somehost.com", port = 12345, pid = Pid, status = free}]], elwhc_sessions:list()),

    %Must select same session as before ....
    ?assertEqual({ok, Pid}, elwhc_sessions:select(http, "www.somehost.com", 12345, 10)),

    ?assertMatch([[#elwhc_session{scheme = http, host = "www.somehost.com", port = 12345, pid = Pid, status = in_use}]], elwhc_sessions:list()),

    %Now, get another session 
    Result2 =  elwhc_sessions:select(http, "www.somehost.com", 12345, 10),
    ?assertMatch({ok, Pid2} when is_pid(Pid2), Result2),

    {ok, Pid2} = Result2,

    ?assertMatch([
            [#elwhc_session{scheme = http, host = "www.somehost.com", port = 12345, pid = Pid, status = in_use}]
        ,   [#elwhc_session{scheme = http, host = "www.somehost.com", port = 12345, pid = Pid2, status = in_use}]
    ], elwhc_sessions:list()),

    ?assertEqual({error, max_sessions}, elwhc_sessions:select(http, "www.somehost.com", 12345, 2)),

    elwhc_test_helper:kill(Pid, kill),

    elwhc_test_helper:kill(Pid2, kill),

    ok.


teardown_test() ->

    elwhc_test_helper:kill(elwhc_sessions, shutdown),

    ok.



%EOF
