-module(elwhc).

-include("elwhc_private.hrl").

-export([request/5, request/6]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(request_guard(M, U, B, H, O), (is_list(Url) andalso

                                                    is_binary(Body) andalso

                                                    is_list(Headers) andalso

                                                    is_list(Options) andalso

                                                   (Method =:= 'GET' orelse
                                                    Method =:= 'POST' orelse
                                                    Method =:= 'PUT' orelse
                                                    Method =:= 'DELETE' orelse
                                                    Method =:= 'HEAD' orelse
                                                    Method =:= 'OPTIONS'))).

-define(handler_pid_guard(P), (is_pid(HandlerPid) orelse (HandlerPid =:= undefined))).

-spec request(http_method(), string(), binary(), http_headers(), http_options()) -> elwhc_request_result().
request(Method, Url, Body, Headers, Options) ->
    request(Method, Url, Body, Headers, Options, undefined).

-spec request(http_method(), string(), binary(), http_headers(), http_options(), elwhc_handler_pid()) -> elwhc_request_result().
request(Method, Url, Body, Headers, Options, HandlerPid) when ?request_guard(Method, Url, Body, Headers, Options) andalso ?handler_pid_guard(HandlerPid) ->

    case parse_url(Url) of
    {ok, PUrl} ->
        Opts = build_options(Options, #elwhc_opts{}),

        Req = #elwhc_request{method = Method, purl = PUrl, body = Body, headers = Headers, options = Opts},

        if (is_pid(HandlerPid)) ->
            %"Standalone" mode ...
            elwhc_handler:request(HandlerPid, Req);
        true ->
            MaxSessions = Opts#elwhc_opts.max_sessions,

            Scheme  = PUrl#parsed_url.scheme,
            Host    = PUrl#parsed_url.host,
            Port    = PUrl#parsed_url.port,

            case elwhc_sessions:select(Scheme, Host, Port, MaxSessions) of
            {ok, SessionPid} ->
                Res = elwhc_handler:request(SessionPid, Req),
                elwhc_sessions:free(SessionPid),
                Res;
            {error, max_sessions} ->
                {error, max_sessions}
            end
        end;

    Error ->
        Error
    end.

-spec parse_url(string()) -> {ok, parsed_url()} | {error, invalid_scheme} | {error, malformed_url}.
parse_url(Url) ->

    case re:run(Url, "^(([^:/?#]+):)?(//([^/?#]*))?(([^?#]*)([?](.*))?)?", [{capture, [2, 4, 5], list}]) of
    {match, ["http", UserInfoHostAndPort, PathQueryFragment]} ->
        parse_url(http, UserInfoHostAndPort, PathQueryFragment);
    {match, ["https", UserInfoHostAndPort, PathQueryFragment]} ->
        parse_url(https, UserInfoHostAndPort, PathQueryFragment);
    {match, _} ->
        {error, invalid_scheme}
    end.
        
-spec parse_url(http_scheme(), string(), string()) -> {ok, parsed_url()} | {error, malformed_url}.
parse_url(Scheme, UserInfoHostAndPort, PathQueryFrag) ->

    case re:run(UserInfoHostAndPort, "^((.*)[@])?(([[].+[]])|([^:]+))?([:]([0-9]+))?", [{capture, [2, 3, 7], list}]) of
    {match, [UserInfo, Host, ""]} ->
        Port = default_port(Scheme),
        {ok, ?parsed_url(Scheme, Host, Port, Host, UserInfo, PathQueryFrag)};
    {match, [UserInfo, Host, SPort]} ->
        Port = list_to_integer(SPort),
        {ok, ?parsed_url(Scheme, Host, Port, Host ++ ":" ++ SPort, UserInfo, PathQueryFrag)};
    {match, _} ->
        {error, malformed_url}
    end.

-spec build_options(http_options(), elwhc_opts()) -> elwhc_opts().
build_options([{connect_timeout_ms,V} | T], Opts) when is_integer(V) andalso (V > 0) ->
    build_options(T, Opts#elwhc_opts{connect_timeout_ms = V});

build_options([{request_timeout_ms,V} | T], Opts) when is_integer(V) andalso (V > 0) ->
    build_options(T, Opts#elwhc_opts{request_timeout_ms = V});

build_options([{keepalive_ms,V} | T], Opts) when is_integer(V) andalso (V > 0) ->
    build_options(T, Opts#elwhc_opts{keepalive_ms = V});

build_options([{keepalive,V} | T], Opts) when is_boolean(V) ->
    build_options(T, Opts#elwhc_opts{keepalive = V});

build_options([{tcp_connect_options,V} | T], Opts) when is_list(V) ->
    build_options(T, Opts#elwhc_opts{tcp_connect_options = V});

build_options([{ssl_options,V} | T], Opts) when is_list(V) ->
    build_options(T, Opts#elwhc_opts{ssl_options = V});

build_options([{max_sessions,V} | T], Opts) when is_integer(V) andalso (V > 0) ->
    build_options(T, Opts#elwhc_opts{max_sessions = V});

build_options([{max_requests_per_session,V} | T], Opts) when is_integer(V) andalso (V > 0) ->
    build_options(T, Opts#elwhc_opts{max_requests_per_session = V});

build_options([{stream_from,V} | T], Opts) when is_function(V, 0) orelse is_list(V) ->
    build_options(T, Opts#elwhc_opts{stream_from = V});

build_options([], Opts) -> Opts.
 

-spec default_port(http_scheme()) -> 80 | 443.
default_port(http) -> 80;
default_port(https) -> 443.

%EOF
