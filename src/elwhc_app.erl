-module(elwhc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_dev/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-define(START_DEPS, [asn1, crypto, public_key,ssl]).

start(_StartType, _StartArgs) ->
    elwhc_sup:start_link().

start_dev() ->
    [application:start(D) || D <- ?START_DEPS],
    ok = application:start(elwhc).


stop(_State) ->
    ok.
