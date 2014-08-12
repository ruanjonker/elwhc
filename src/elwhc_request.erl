-module(elwhc_request).

-include("elwhc_private.hrl").

-export([start_link/0,request/2]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(update_ttg(X), update_ttg(X, os:timestamp())).

-spec start_link() -> {ok, pid()}.
start_link() ->
    F =  fun () -> request_worker_loop(#elwhc_request{}) end,
    {ok, spawn_link(F)}.

-spec request(pid(), elwhc_request()) -> {ok,  http_status_code(), http_headers(), binary()} | {error, term()}.
request(Pid, Request) ->

    Ref = make_ref(),
    Caller = self(),

    MRef = monitor(process, Pid),

    Pid ! {elwhc_request, Caller, Ref, Request},

    receive 
    {'DOWN', MRef, process, Pid, Err} ->
        demonitor(MRef, [flush]),
        throw (Err);
    {Ref, Result} ->
        demonitor(MRef, [flush]),
        Result
    end.

-spec request_worker_loop(elwhc_request()) -> ok.
request_worker_loop(PrevRequest) ->

    Opts = PrevRequest#elwhc_request.options,

    KeepAliveMs = Opts#elwhc_opts.keepalive_ms,

    HandledRequest =
    receive {elwhc_request, Caller, Ref, Request}  ->

        do_work(Caller, Ref, Request, PrevRequest)

    after KeepAliveMs ->

        case PrevRequest#elwhc_request.socket of
        {SockMod, Socket} ->
            ok = SockMod:close(Socket);
        undefined ->
            ok
        end,
        PrevRequest#elwhc_request{socket = undefined}
    end,

    request_worker_loop(HandledRequest).

-spec do_work(pid(), reference(), elwhc_request(), elwhc_request()) -> elwhc_request().
do_work(Caller, Ref, Request, PrevRequest) ->

    Opts = Request#elwhc_request.options,

    RequestTimeoutMs = Opts#elwhc_opts.request_timeout_ms,

    ExistingSocket = PrevRequest#elwhc_request.socket,

    PPurl = PrevRequest#elwhc_request.purl,
    RPurl = Request#elwhc_request.purl,

    MustReconnect = (ExistingSocket == undefined) orelse 
                    (PPurl#parsed_url.scheme =/= RPurl#parsed_url.scheme) orelse 
                    (PPurl#parsed_url.host   =/= RPurl#parsed_url.host) orelse
                    (PPurl#parsed_url.port   =/= RPurl#parsed_url.port),

    Result =
    if MustReconnect ->

        %Establish a new connection
        case ExistingSocket of
        {SockMod, Socket} ->
            ok = SockMod:close(Socket);
        undefined ->
            ok
        end,

        handle(connect, Request#elwhc_request{request_ttg_ms = RequestTimeoutMs, request_ttg_t0 = os:timestamp()});

    true ->
        %Re-use existing connection ...
        handle(connected, Request#elwhc_request{socket = ExistingSocket, request_ttg_ms = RequestTimeoutMs, request_ttg_t0 = os:timestamp()})
    end,
    
    CallerRsp = 
    case Result of
    {ok, HandledRequest} ->
        {ok, HandledRequest#elwhc_request.rsp_status, HandledRequest#elwhc_request.rsp_headers, HandledRequest#elwhc_request.rsp_body};
    {Error, _} ->
        Error
    end,

    Caller ! {Ref, CallerRsp},

    {_, HandledRequest0} = Result,    

    do_work_post_processing(HandledRequest0).

do_work_post_processing(Request) ->

    R0 =
    case Request#elwhc_request.socket of
    undefined ->
        Request;
    {SockMod, Sock} ->

        Opts = Request#elwhc_request.options,

        ClientWantToClose = Opts#elwhc_opts.keepalive =/= true,

        ServerWantToClose = lists:keyfind('Connection', 1, Request#elwhc_request.rsp_headers) == {'Connection', "close"},

        if (ClientWantToClose orelse ServerWantToClose) ->
            ok = SockMod:close(Sock),
            Request#elwhc_request{socket = undefined};
        true ->
            Request
        end

    end,

    R0#elwhc_request{rsp_status = undefined, rsp_headers = [], rsp_body = <<>>}.

-spec handle(elwhc_request_state(), elwhc_request()) -> {ok, http_status_code(), http_headers(), binary()} | {error, term()}.
handle(connect, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    #parsed_url {scheme = Scheme, host = Host, port = Port} = Request#elwhc_request.purl,

    Opts = Request#elwhc_request.options,

    ConnectTimeoutMs = min(Opts#elwhc_opts.connect_timeout_ms, TtgMs),
    TcpConnectOptions = Opts#elwhc_opts.tcp_connect_options,

    case gen_tcp:connect(Host, Port, TcpConnectOptions, ConnectTimeoutMs) of
    {ok, Sock} ->
        ok = inet:setopts(Sock, [binary, {send_timeout, ConnectTimeoutMs}, {send_timeout_close, true}]),
        if (Scheme =:= https) ->
            handle(ssl_connect, ?update_ttg(Request#elwhc_request{socket = {gen_tcp, Sock}}));
        true ->
            handle(connected, ?update_ttg(Request#elwhc_request{socket = {gen_tcp, Sock}}))
        end;
    Error ->
        {Error, Request}
    end;

handle(ssl_connect, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    {_, Sock} = Request#elwhc_request.socket,

    Opts = Request#elwhc_request.options,

    ConnectTimeoutMs = min(Opts#elwhc_opts.connect_timeout_ms, TtgMs),

    SslOptions = Opts#elwhc_opts.ssl_options,

    case ssl:connect(Sock, SslOptions, ConnectTimeoutMs) of
    {ok, SslSock} ->
        handle(connected, ?update_ttg(Request#elwhc_request{socket = {ssl, SslSock}}));
    Error ->
        {Error, Request}
    end;

handle(connected, Request) ->

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
        ,   {"User-Agent",      "ELWHC/1.0"}
    ]

    ++ if (Opts#elwhc_opts.keepalive) -> [{"Connection", "keep-alive"}]; true -> [] end  

    ++ Request#elwhc_request.headers, []),
 
    SendBody = (Request#elwhc_request.method =:= 'POST') orelse (Request#elwhc_request.method =:= 'PUT'), 

    case SockMod:send(Sock, [ReqLine, Headers, "\r\n", if (SendBody) -> Body; true -> <<>> end]) of
    ok ->
        handle(rx_rsp_line, ?update_ttg(Request));
    Error ->
        {Error, Request}
    end;

handle(rx_rsp_line, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    ok = inet_setopts(Request#elwhc_request.socket, [{packet, http}]),

    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, RspHttpPacket} ->
        handle(rx_headers, ?update_ttg(Request#elwhc_request{rsp_status = RspHttpPacket}));
    Error ->
        {Error, Request}
    end;

handle(rx_headers, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, {http_header, _, HttpField, _, HttpString}} ->

        RspHeaders = Request#elwhc_request.rsp_headers,

        NewRspHeaders = [{HttpField, HttpString} | RspHeaders],

        handle(rx_headers, ?update_ttg(Request#elwhc_request{rsp_headers = NewRspHeaders}));

    {ok, http_eoh} ->
        ok = inet_setopts(Request#elwhc_request.socket, [{packet, raw}]),

        handle(plan_rx_body, ?update_ttg(Request));

    {error, _} = Err ->
        {Err, Request}
    end;

handle(plan_rx_body, #elwhc_request{request_ttg_ms = TtgMs, rsp_status = RspHttpPacket, rsp_headers = RspHeaders} = Request) when (TtgMs > 0) ->

    {http_response, _, StatusCode, _} = RspHttpPacket,

    if ((StatusCode >= 100) andalso (StatusCode < 300)) ->

        case lists:keysearch('Content-Length', 1, RspHeaders) of
        {value, {_, StringContentLength}} ->
            handle(rx_body_content_length, Request#elwhc_request{content_length = list_to_integer(StringContentLength)});
        false ->
            case lists:keysearch('Transfer-Encoding', 1, RspHeaders) of
            {value, {_,_}} ->
                handle(rx_body_chunked, Request#elwhc_request{content_length = undefined});
            false ->
                handle(rx_body_until_connection_close, Request#elwhc_request{content_length = undefined})
            end
        end;

    true ->
        handle(rx_body_done, Request#elwhc_request{content_length = 0})
    end;

handle(rx_body_content_length, #elwhc_request{request_ttg_ms = TtgMs, rsp_body = RspBodySoFar, content_length = ContentLength} = Request) when (TtgMs > 0) andalso
                                                                                                                             ((is_integer(ContentLength)) andalso (ContentLength > 0)) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    case SockMod:recv(Sock, ContentLength, TtgMs) of
    {ok, RspBody} ->

        NewContentLength = ContentLength - size(RspBody),

        handle(rx_body_content_length, ?update_ttg(Request#elwhc_request{rsp_body = <<RspBodySoFar/binary,RspBody/binary>>, content_length = NewContentLength}));

    Error ->
        {Error, Request}

    end;

handle(rx_body_content_length, #elwhc_request{request_ttg_ms = TtgMs, content_length = ContentLength} = Request) when (TtgMs > 0) andalso (ContentLength =:= 0) ->
    {ok, Request};

handle(rx_body_until_connection_close, #elwhc_request{request_ttg_ms = TtgMs, rsp_body = RspBodySoFar} = Request) when (TtgMs > 0) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, RspBody} ->

        handle(rx_body_until_connection_close, ?update_ttg(Request#elwhc_request{rsp_body = <<RspBodySoFar/binary,RspBody/binary>>}));

    {error, closed} ->
        {ok, Request#elwhc_request{socket = undefined}};

    Error ->
        {Error, Request}

    end;

handle(rx_body_chunked, #elwhc_request{request_ttg_ms = TtgMs, rsp_body = RspBodySoFar} = Request) when (TtgMs > 0) ->
    {{error, not_implemented}, Request};


handle(_, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs =< 0) ->
    {{error, request_timeout}, Request}.


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
