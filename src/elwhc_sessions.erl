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
        select/4,
        free/1,
        list/0
        ]).

-define(scheme_guard(S), ((Scheme =:= http) orelse (Scheme =:= https))). 
-define(max_sessions_guard(M), (is_integer(M) andalso (M > 0))).
-define(match_by_pid(P), #elwhc_session{key = '_', scheme = '_', host = '_', port = '_', status = '_', ts = '_', pid = P}).

-record(state, {}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec select(http_scheme(), http_host(), http_port(), pos_integer()) -> ok. 
select(Scheme, Host, Port, MaxSessions) when ?scheme_guard(Scheme) andalso ?max_sessions_guard(MaxSessions) ->
    gen_server:call(?MODULE, {select, Scheme, Host, Port, MaxSessions}, infinity).

-spec free(pid()) -> ok.
free(Pid) ->
    gen_server:call(?MODULE, {free, Pid}, infinity).

-spec list() -> list(elwhc_session()).
list() ->
    ets:match(?elwhc_sessions_table, '$1').

init(_) ->
    process_flag(trap_exit, true),
    ets:new(?elwhc_sessions_table, [bag, named_table, protected, {keypos, #elwhc_session.key}]),
    {ok, #state{}}.

handle_call({select, Scheme, Host, Port, MaxSessions}, _From, State) ->

    TableKey = {Scheme, Host, Port},

    Sessions = ets:lookup(?elwhc_sessions_table, TableKey),

    FreeSessions = [Session || #elwhc_session{status = S} = Session <- Sessions, S =:= ?elwhc_session_free],

    Result = 
    case FreeSessions of
    [Obj | _] ->

        true = ets:delete_object(?elwhc_sessions_table, Obj),

        NObj = Obj#elwhc_session{status = ?elwhc_session_in_use, ts = os:timestamp()},

        true = ets:insert(?elwhc_sessions_table, NObj),

        {ok, NObj#elwhc_session.pid};

    [] ->

        if (MaxSessions > length(Sessions)) ->
            {ok, Pid} = elwhc_handler:start_link(false),
            true = ets:insert(?elwhc_sessions_table, #elwhc_session{key = TableKey, scheme = Scheme, host = Host, port = Port, pid = Pid}),
            {ok, Pid};
        true ->
            {error, max_sessions}
        end
    end,
    {reply, Result, State};

handle_call({free, Pid}, _From, State) ->

    case ets:match_object(?elwhc_sessions_table, ?match_by_pid(Pid)) of
    [Obj] ->
        true = ets:delete_object(?elwhc_sessions_table, Obj),

        NObj = Obj#elwhc_session{status = ?elwhc_session_free, ts = os:timestamp()}, 

        true = ets:insert(?elwhc_sessions_table, NObj);
    _ ->
        ok
    end,        
    {reply, ok, State};

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _}, State) ->
    true = ets:match_delete(?elwhc_sessions_table, ?match_by_pid(Pid)),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.




%EOF
