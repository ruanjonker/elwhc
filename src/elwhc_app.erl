-module(elwhc_app).
-behaviour(application).

-include("elwhc_private.hrl").

-export([start/2, stop/1, start_dev/0]).

-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    elwhc_sup:start_link().

-spec start_dev() -> ok.
start_dev() ->
    [application:start(D) || D <- ?ELWHC_START_DEPS],
    ok = application:start(elwhc).

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%EOF
