-module(elwhc_request).

-include("elwhc_private.hrl").

-export([request/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-spec request(elwhc_request()) -> {ok,  http_status_code(), http_headers(), binary()} | {error, term()}.
request(Request) ->
    request(connect, Request).

-spec request(elwhc_request_state(), elwhc_request()) -> {ok, http_status_code(), http_headers(), binary()} | {error, term()}.
request(connect, Request) ->

    #parsed_url {scheme = Scheme, host = Host, port = Port} = Request#elwhc_request.purl,

    Opts = Request#elwhc_request.options,

    ConnectTimeoutMs = Opts#elwhc_opts.connect_timeout_ms,
    TcpConnectOptions = Opts#elwhc_opts.tcp_connect_options,

    case gen_tcp:connect(Host, Port, TcpConnectOptions, ConnectTimeoutMs) of
    {ok, Sock} ->
        if (Scheme =:= https) ->
            SslOptions = Opts#elwhc_opts.ssl_options,
            case ssl:connect(Sock, SslOptions, ConnectTimeoutMs) of
            {ok, SslSock} ->
                request(connected, Request#elwhc_request{socket = {ssl, SslSock}});
            Error ->
                Error
            end;
        true ->
            request(connected, Request#elwhc_request{socket = {gen_tcp, Sock}})
        end;
    Error ->
        Error
    end;

request(connected, Request) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    Opts = Request#elwhc_request.options,

    Method = atom_to_list(Request#elwhc_request.method),

    PUrl = Request#elwhc_request.purl,

    PathQueryFrag = PUrl#parsed_url.path_query_frag,

    HttpVer = if (Opts#elwhc_opts.keepalive) -> "HTTP/1.1"; true -> "HTTP/1.0" end,

    Body = Request#elwhc_request.body,
    
    ReqLine = [Method," ",PathQueryFrag," ",HttpVer,"\r\n"],

    Headers = build_headers([{"content-length", integer_to_list(size(Body))}, {"User-Agent", "elwhc v1.0.0;Hit it hard!"}] ++ Request#elwhc_request.headers, []),
 
    RequestTimeoutMs = Opts#elwhc_opts.request_timeout_ms,

    case SockMod:send(Sock, [ReqLine, Headers, Body], RequestTimeoutMs) of
    ok ->
        ok = inet:setopts(Sock, [{packet, http}]),
        case SockMod:recv(Sock, 0, RequestTimeoutMs) of
        {ok, HttpPacket} ->
            case SockMod:recv(Sock, 0, RequestTimeoutMs) of
            {ok, HttpHeaders} ->
                %TODO: Receive rest of body etc.
                SockMod:close(Sock),
                {ok, HttpPacket, HttpHeaders};
            Error ->
                SockMod:close(Sock),
                Error
            end;
        Error ->
            SockMod:close(Sock),
            Error
        end;

    Error ->
        SockMod:close(Sock),
        Error
    end.

build_headers([], Headers) -> Headers;
build_headers([{H,V} | T], Headers) -> 
    NewHeaders = [H,":",V,"\r\n" | Headers],
    build_headers(T, NewHeaders).

%EOF
