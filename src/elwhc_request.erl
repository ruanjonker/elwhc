-module(elwhc_request).

-include("elwhc_private.hrl").

-export([request/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(update_ttg(X), update_ttg(X, os:timestamp())).

-spec request(elwhc_request()) -> {ok,  http_status_code(), http_headers(), binary()} | {error, term()}.
request(Request) ->

    Opts = Request#elwhc_request.options,

    RequestTimeoutMs = Opts#elwhc_opts.request_timeout_ms,

    Ref = make_ref(),
    Caller = self(),

    F = fun() ->
        Res = request(connect, Request#elwhc_request{request_ttg_ms = RequestTimeoutMs, request_ttg_t0 = os:timestamp()}),
        Caller ! {Ref, Res}
    end,

    {MRef, Pid} = spawn_monitor(F),

    receive 
    {'DOWN', MRef, process, Pid, Err} ->
        demonitor(Ref, [flush]),
        throw (Err);
    {Ref, Result} ->
        demonitor(Ref, [flush]),
        Result
    end.


-spec request(elwhc_request_state(), elwhc_request()) -> {ok, http_status_code(), http_headers(), binary()} | {error, term()}.
request(connect, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    #parsed_url {scheme = Scheme, host = Host, port = Port} = Request#elwhc_request.purl,

    Opts = Request#elwhc_request.options,

    ConnectTimeoutMs = min(Opts#elwhc_opts.connect_timeout_ms, TtgMs),
    TcpConnectOptions = Opts#elwhc_opts.tcp_connect_options,

    case gen_tcp:connect(Host, Port, TcpConnectOptions, ConnectTimeoutMs) of
    {ok, Sock} ->
        ok = inet:setopts(Sock, [binary, {send_timeout, ConnectTimeoutMs}, {send_timeout_close, true}]),
        if (Scheme =:= https) ->
            request(ssl_connect, ?update_ttg(Request#elwhc_request{socket = {gen_tcp, Sock}}));
        true ->
            request(connected, ?update_ttg(Request#elwhc_request{socket = {gen_tcp, Sock}}))
        end;
    Error ->
        Error
    end;

request(ssl_connect, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    {_, Sock} = Request#elwhc_request.socket,

    Opts = Request#elwhc_request.options,

    ConnectTimeoutMs = min(Opts#elwhc_opts.connect_timeout_ms, TtgMs),

    SslOptions = Opts#elwhc_opts.ssl_options,

    case ssl:connect(Sock, SslOptions, ConnectTimeoutMs) of
    {ok, SslSock} ->
        request(connected, ?update_ttg(Request#elwhc_request{socket = {ssl, SslSock}}));
    Error ->
        Error
    end;

request(connected, Request) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    Opts = Request#elwhc_request.options,

    Method = atom_to_list(Request#elwhc_request.method),

    PUrl = Request#elwhc_request.purl,

    HostHeader = PUrl#parsed_url.raw_host,

    PathQueryFrag = PUrl#parsed_url.path_query_frag,

    HttpVer = if (Opts#elwhc_opts.keepalive) -> "HTTP/1.1"; true -> "HTTP/1.0" end,

    Body = Request#elwhc_request.body,
    
    AbsPath = 
    case PathQueryFrag of
    "" -> "/";
    _ -> PathQueryFrag
    end,

    ReqLine = [Method," ", AbsPath," ",HttpVer,"\r\n"],

    Headers = build_headers([
            {"Host",            HostHeader}
        ,   {"Content-Length",  integer_to_list(size(Body))}
        ,   {"User-Agent",      "elwhc v1.0.0;Hit it hard!"}

    ] ++ Request#elwhc_request.headers, []),
 
    case SockMod:send(Sock, [ReqLine, Headers, "\r\n", Body]) of
    ok ->
        request(rx_rsp_line, ?update_ttg(Request));
    Error ->
        Error
    end;

request(rx_rsp_line, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    ok = inet_setopts(Request#elwhc_request.socket, [{packet, http}]),

    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, RspHttpPacket} ->
        request(rx_headers, ?update_ttg(Request#elwhc_request{rsp_status = RspHttpPacket}));
    Error ->
        Error
    end;

request(rx_headers, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, {http_header, _, HttpField, _, HttpString}} ->

        RspHeaders = Request#elwhc_request.rsp_headers,

        NewRspHeaders = [{HttpField, HttpString} | RspHeaders],

        request(rx_headers, ?update_ttg(Request#elwhc_request{rsp_headers = NewRspHeaders}));

    {ok, http_eoh} ->
        ok = inet_setopts(Request#elwhc_request.socket, [{packet, raw}]),
        request(rx_body, ?update_ttg(Request));
        
    {error, _} = Err ->
        Err
    end;

request(rx_body, #elwhc_request{request_ttg_ms = TtgMs, rsp_body = RspBodySoFar} = Request) when (TtgMs > 0) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    %TODO Keep on receiving to either Conent-Length reached, socket_close or Ttg expire/rx_timeout hit ...
    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, RspBody} ->

        request(rx_body, ?update_ttg(Request#elwhc_request{rsp_body = <<RspBodySoFar/binary,RspBody/binary>>}));

    {error, closed} ->

        ok = SockMod:close(Sock),

        {ok, Request};

    Error ->
        Error

    end;


request(_, #elwhc_request{request_ttg_ms = TtgMs} = _Request) when (TtgMs =< 0) ->
        
    {error, request_timeout}.



-spec update_ttg(elwhc_request(), erlang:timestamp()) -> elwhc_request().
update_ttg(#elwhc_request{request_ttg_ms = RequestTTGMs, request_ttg_t0 = T0} = Request, T1) ->
    NewRequestTTGMs =  RequestTTGMs - trunc(timer:now_diff(T1, T0)/1000),
    if (NewRequestTTGMs > 0) ->
        Request#elwhc_request{request_ttg_ms = NewRequestTTGMs, request_ttg_t0 = T1};
    true ->
        Request#elwhc_request{request_ttg_ms = 0, request_ttg_t0 = T1}
    end.

-spec inet_setopts({gen_tcp, term()} | {ssl, term()}, list(term())) -> ok.
inet_setopts({gen_tcp, Sock}, Opts) -> inet:setopts(Sock, Opts);
inet_setopts({ssl, Sock}, Opts) -> ssl:setopts(Sock, Opts).

build_headers([], Headers) -> Headers;
build_headers([{H,V} | T], Headers) -> 
    NewHeaders = [H,":",V,"\r\n" | Headers],
    build_headers(T, NewHeaders).

%EOF
