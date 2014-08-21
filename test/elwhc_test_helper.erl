-module(elwhc_test_helper).

-compile([export_all]).

kill(Name, How) when is_atom(Name) and ((How =:= shutdown) or (How =:= kill)) ->
    kill(whereis(Name), How);

kill({global, Name}, How) when ((How =:= shutdown) or (How =:= kill)) ->
    kill(global:whereis_name(Name), How);

kill(Pid, How) when is_pid(Pid) and ((How =:= shutdown) or (How =:= kill)) ->
    unlink(Pid),
    monitor(process, Pid),
    exit(Pid, How),
    receive {'DOWN', _, process, Pid, _} -> ok end.

%EOF
