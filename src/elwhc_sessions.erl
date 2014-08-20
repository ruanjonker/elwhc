-module(elwhc_sessions).
-behavior(gen_server).

-include("elwhc_private.hrl").

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
        ]).

-export([
        start_link/0,
        get/4
        ]).

-define(scheme_guard(S), ((Scheme =:= http) orelse (Scheme =:= https))). 
-define(max_sessions_guard(M), ((M =:= infinity) orelse ((is_integer(M)) andalso (M > 0)))).
-record(state, {}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(http_scheme(), http_host_port(), pid(), infinity | pos_integer()) -> ok. 
get(Scheme, HostPort, Pid, MaxSessions) when is_pid(Pid) andalso ?scheme_guard(Scheme) andalso ?max_sessions_guard(MaxSessions) ->
    gen_server:call(?MODULE, {get, Scheme, HostPort, Pid, MaxSessions}, infinity).

init(_) ->
    ets:new(?elwhc_sessions_table, [named_table, protected, {keypos, #elwhc_session.key + 1}]),
    {ok, #state{}}.

handle_call({get, Scheme, HostPort, Pid, MaxSessions}, _From, State) ->

    TableKey = {Scheme, HostPort, Pid},

    case ets:lookup(?elwhc_sessions_table, TableKey) of
    [#elwhc_session{}] ->
        ok;
    [] ->
        ok = ets:insert(?elwhc_sessions_table, #elwhc_session{key = TableKey, scheme = Scheme, host_port = HostPort, pid = Pid}),
        monitor(process, Pid)
    end,
    {reply, ok, State};

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    demonitor(Ref, [flush]),
    true = ets:match_delete(?elwhc_sessions_table, #elwhc_session{key = '_', scheme = '_', host_port = '_', pid = Pid}),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.




%EOF
