-module(elwhc_test_helper).

-include("elwhc_private.hrl").

-compile([export_all]).

stop_app_deps() ->
    [application:stop(A) || A <- lists:reverse(?ELWHC_START_DEPS)],
    ok.

start_app_deps() ->
    [ok = application:start(A) || A <- ?ELWHC_START_DEPS],
    ok.

kill(Name, How) when is_atom(Name) and ((How =:= shutdown) or (How =:= kill)) ->
    kill(whereis(Name), How);

kill(Pid, How) when is_pid(Pid) and ((How =:= shutdown) or (How =:= kill)) ->
    unlink(Pid),
    monitor(process, Pid),
    exit(Pid, How),
    receive {'DOWN', _, process, Pid, _} -> ok end.

%EOF
