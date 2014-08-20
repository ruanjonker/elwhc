-module(elwhc_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->

    ChildSpecs = [
        {sessions, {elwhc_sessions, start_link, []},
            permanent, 60000, worker, [elwhc_sessions]}
    ],

    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

%EOF
