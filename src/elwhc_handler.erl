-module(elwhc_handler).

-include("elwhc_private.hrl").

-export([start_link/1,request/2]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(update_ttg(X), update_ttg(X, os:timestamp())).

-spec start_link(boolean()) -> {ok, pid()}.
start_link(Standalone) when is_boolean(Standalone) ->
    F =  fun () -> request_worker_loop(#elwhc_request{standalone = Standalone}) end,
    {ok, spawn_link(F)}.

-spec request(pid(), elwhc_request()) -> {ok,  http_status_code(), http_rsp_headers(), binary()} | {error, term()}.
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

    if (HandledRequest#elwhc_request.standalone) ->
        %Keep the process alive, we are in standalone mode
        ok;
    true ->
        if (HandledRequest#elwhc_request.socket =:= undefined) ->
            exit(normal);
        true ->
            ok
        end
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

        handle(connect, Request#elwhc_request{socket = undefined, request_ttg_ms = RequestTimeoutMs, request_ttg_t0 = os:timestamp()});

    true ->
        %Re-use existing connection ...
        handle(connected, Request#elwhc_request{socket = ExistingSocket, request_ttg_ms = RequestTimeoutMs, request_ttg_t0 = os:timestamp()})
    end,
    
    {ReqStatus, CallerRsp, HandledRequest0} = 
    case Result of
    {ok, HandledRequest} ->

        {http_response,_,SC,_} = HandledRequest#elwhc_request.rsp_status,

        NewReqCount = PrevRequest#elwhc_request.request_count + 1,
        
        RH = HandledRequest#elwhc_request.rsp_headers,
        RB = HandledRequest#elwhc_request.rsp_body,

        {ok, {ok, SC, RH, RB}, HandledRequest#elwhc_request{request_count = NewReqCount}};

    {Error, HandledRequest} ->
        {error, Error, HandledRequest}
    end,

    Caller ! {Ref, CallerRsp},

    do_work_post_processing(ReqStatus, HandledRequest0).

-spec do_work_post_processing(ok | error, elwhc_request()) -> elwhc_request().
do_work_post_processing(ReqStatus, Request) ->

    R0 =
    case Request#elwhc_request.socket of
    undefined ->
        Request;
    {SockMod, Sock} ->

        Opts = Request#elwhc_request.options,

        MaxRequestsReached = Request#elwhc_request.request_count >= Opts#elwhc_opts.max_requests_per_session,

        ClientWantToClose = Opts#elwhc_opts.keepalive =/= true,

        ServerWantToClose = lists:keyfind("Connection", 1, Request#elwhc_request.rsp_headers) == {"Connection", "close"},

        if (ClientWantToClose orelse ServerWantToClose orelse MaxRequestsReached orelse (ReqStatus =:= error)) ->
            ok = SockMod:close(Sock),
            Request#elwhc_request{socket = undefined, request_count = 0};
        true ->
            Request
        end

    end,

    R0#elwhc_request{rsp_status = undefined, rsp_headers = [], rsp_body = <<>>}.

-spec handle(elwhc_request_state(), elwhc_request()) -> {ok, http_status_code(), http_headers(), binary()} | {error, term()}.
handle(connect, #elwhc_request{socket = undefined, request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    #parsed_url {scheme = Scheme, host = Host, port = Port} = Request#elwhc_request.purl,

    Opts = Request#elwhc_request.options,

    ConnectTimeoutMs = min(Opts#elwhc_opts.connect_timeout_ms, TtgMs),
    TcpConnectOptions = Opts#elwhc_opts.tcp_connect_options,

    case gen_tcp:connect(Host, Port, TcpConnectOptions, ConnectTimeoutMs) of
    {ok, Sock} ->
        ok = inet:setopts(Sock, [binary, {send_timeout, TtgMs}, {send_timeout_close, true}]),
        if (Scheme =:= https) ->
            handle(ssl_connect, ?update_ttg(Request#elwhc_request{socket = {gen_tcp, Sock}}));
        true ->
            handle(connected, ?update_ttg(Request#elwhc_request{socket = {gen_tcp, Sock}}))
        end;
    {error, timeout} ->
        {{error, connect_timeout}, Request};
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

    HostHeaderValue = PUrl#parsed_url.raw_host,

    PathQueryFrag = PUrl#parsed_url.path_query_frag,

    HttpVer = if (Opts#elwhc_opts.keepalive) -> "HTTP/1.1"; true -> "HTTP/1.0" end,

    Body = Request#elwhc_request.body,
    
    AbsPath = 
    case PathQueryFrag of
    "" -> "/";
    _ -> PathQueryFrag
    end,

    ReqLine = [Method," ", AbsPath," ",HttpVer,"\r\n"],

    UserHeaders = Request#elwhc_request.headers,
    KeepAlive = Opts#elwhc_opts.keepalive,

    SendBody = ((Request#elwhc_request.method =:= 'POST') orelse (Request#elwhc_request.method =:= 'PUT')),

    {NextState, TxPayload} =
    if SendBody andalso is_function(Opts#elwhc_opts.stream_from, 0) ->
        Headers = build_headers(merge_headers(HostHeaderValue, KeepAlive, undefined, UserHeaders), []),
        {stream_from_fun, [ReqLine, Headers, "\r\n"]};
    true ->
        ContentLength = if SendBody -> size(Body) ; true -> 0 end,
        Headers = build_headers(merge_headers(HostHeaderValue, KeepAlive, ContentLength, UserHeaders), []),
        {rx_rsp_line, [ReqLine, Headers, "\r\n", if (SendBody) -> Body; true -> <<>> end]}
    end,

    case SockMod:send(Sock, TxPayload) of
    ok ->
        handle(NextState, ?update_ttg(Request));
    {error, timeout} ->
        {{error, send_timeout}, Request};
    Error ->
        {Error, Request}
    end;

handle(stream_from_fun, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    {SockMod, Sock} = Request#elwhc_request.socket,

    Opts = Request#elwhc_request.options,

    Fun = Opts#elwhc_opts.stream_from,

    case Fun() of
    Binary when is_binary(Binary) ->

        BinSize = size(Binary),

        HexSize = integer_to_list(BinSize, 16),

        case SockMod:send(Sock, [HexSize, <<"\r\n">>, Binary, <<"\r\n">>]) of
        ok ->
            NextState = if (BinSize > 0) -> stream_from_fun; true -> rx_rsp_line end,
            handle(NextState, ?update_ttg(Request));
        {error, timeout} ->
            {{error, send_timeout}, Request};
        Error ->
            {Error, Request}
        end

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

        LHttpField = maybe_atom_to_list(HttpField),

        NewRspHeaders = [{string:to_lower(LHttpField), LHttpField, HttpString} | RspHeaders],

        handle(rx_headers, ?update_ttg(Request#elwhc_request{rsp_headers = NewRspHeaders}));

    {ok, http_eoh} ->
        ok = inet_setopts(Request#elwhc_request.socket, [{packet, raw}]),

        NewRspHeaders = lists:reverse(Request#elwhc_request.rsp_headers),

        handle(plan_rx_body, ?update_ttg(Request#elwhc_request{rsp_headers = NewRspHeaders}));

    {error, _} = Err ->
        {Err, Request}
    end;

handle(plan_rx_body, #elwhc_request{request_ttg_ms = TtgMs, rsp_status = RspHttpPacket, rsp_headers = RspHeaders} = Request) when (TtgMs > 0) ->

    {http_response, _, StatusCode, _} = RspHttpPacket,

    if ((StatusCode >= 100) andalso (StatusCode < 300)) ->

        case lists:keyfind("content-length", 1, RspHeaders) of
        {_, _, StringContentLength} ->
            handle(rx_body_content_length, Request#elwhc_request{content_length = list_to_integer(StringContentLength)});
        false ->
            case lists:keyfind("transfer-encoding", 1, RspHeaders) of
            {_, TransferEncodingHeader, TransferEncodingValue} ->
                case re:run(TransferEncodingValue, "chunked", [{capture, none}]) of
                match ->
                    NewTransferEncodingValue = re:replace(TransferEncodingValue, "([ ]?,)?chunked", "", [{return, list}]),
                    NewHeaderTuple = {"transfer-encoding", TransferEncodingHeader, NewTransferEncodingValue},
                    NewRspHeaders = lists:keyreplace("transfer-encoding", 1, RspHeaders, NewHeaderTuple),
                    handle(rx_body_chunked_length, Request#elwhc_request{content_length = undefined, rsp_headers = NewRspHeaders});
                nomatch ->
                    handle(rx_body_until_connection_close, Request#elwhc_request{content_length = undefined})
                end;
            false ->
                handle(rx_body_until_connection_close, Request#elwhc_request{content_length = undefined})
            end
        end;

    true ->
        {ok, Request#elwhc_request{content_length = 0}}
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

handle(rx_body_chunked_length, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    %See 19.4.6 in https://www.ietf.org/rfc/rfc2616.txt

    {SockMod, Sock} = RSock = Request#elwhc_request.socket,

    ok = inet_setopts(RSock, [{packet, line}]),

    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, RspBody} ->

        {match, [ChunkSize]} = re:run(RspBody, <<"^([0-9a-fA-F]+)">>, [{capture, [1], binary}]),

        IntChunkSize = list_to_integer(binary_to_list(ChunkSize), 16),

        if (IntChunkSize > 0) ->
            handle(rx_body_chunked_entity_body, ?update_ttg(Request#elwhc_request{chunk_bytes_to_go = IntChunkSize}));
        true ->
            handle(rx_body_chunked_entity_headers, ?update_ttg(Request#elwhc_request{chunk_bytes_to_go = 0}))
        end;

    Error ->
        {Error, Request}
    end;

handle(rx_body_chunked_entity_body, #elwhc_request{request_ttg_ms = TtgMs, rsp_body = RspBodySoFar, chunk_bytes_to_go = BytesToGo} = Request) when (TtgMs > 0) ->

    %See 19.4.6 in https://www.ietf.org/rfc/rfc2616.txt

    {SockMod, Sock} = RSock = Request#elwhc_request.socket,

    ok = inet_setopts(RSock, [{packet, 0}]),

    case SockMod:recv(Sock, BytesToGo, TtgMs) of
    {ok, RspBody} ->

        NewBytesToGo = BytesToGo - size(RspBody),

        if (NewBytesToGo > 0) ->
            handle(rx_body_chunked_entity_body, ?update_ttg(Request#elwhc_request{rsp_body = <<RspBodySoFar/binary,RspBody/binary>>, chunk_bytes_to_go = NewBytesToGo}));
        true ->
            {ok, _} = SockMod:recv(Sock, 2, TtgMs), %Read the trailing \r\n after the chunkdata
            handle(rx_body_chunked_length, ?update_ttg(Request#elwhc_request{rsp_body = <<RspBodySoFar/binary,RspBody/binary>>, chunk_bytes_to_go = 0}))
        end;

    Error ->
        {Error, Request}
    end;

handle(rx_body_chunked_entity_headers, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs > 0) ->

    %See 19.4.6 in https://www.ietf.org/rfc/rfc2616.txt

    {SockMod, Sock} = RSock = Request#elwhc_request.socket,

    ok = inet_setopts(RSock, [{packet, httph}]),

    case SockMod:recv(Sock, 0, TtgMs) of
    {ok, {http_header, _, HttpField, _, HttpString}} ->

        RspHeaders = Request#elwhc_request.rsp_headers,

        LHeaderField = maybe_atom_to_list(HttpField),

        NewRspHeaders = [{string:to_lower(LHeaderField), LHeaderField, HttpString} | RspHeaders],

        handle(rx_body_chunked_entity_headers, ?update_ttg(Request#elwhc_request{rsp_headers = NewRspHeaders}));

    {ok, http_eoh} ->
        {ok, Request};

    {error, _} = Err ->
        {Err, Request}

    end;

handle(_, #elwhc_request{request_ttg_ms = TtgMs} = Request) when (TtgMs =< 0) ->
    {{error, timeout}, Request}.

-spec update_ttg(elwhc_request(), erlang:timestamp()) -> elwhc_request().
update_ttg(#elwhc_request{request_ttg_ms = RequestTTGMs, request_ttg_t0 = T0} = Request, T1) ->
    NewRequestTTGMs =  RequestTTGMs - (timer:now_diff(T1, T0) div 1000),
    if (NewRequestTTGMs > 0) ->
        Request#elwhc_request{request_ttg_ms = NewRequestTTGMs, request_ttg_t0 = T1};
    true ->
        Request#elwhc_request{request_ttg_ms = 0, request_ttg_t0 = T1}
    end.

-spec inet_setopts({gen_tcp, term()} | {ssl, term()}, list(term())) -> ok.
inet_setopts({gen_tcp, Sock}, Opts) -> inet:setopts(Sock, Opts);
inet_setopts({ssl, Sock}, Opts) -> ssl:setopts(Sock, Opts).

-spec build_headers(list({string(), string()}), iolist()) -> iolist().
build_headers([{H,V} | T], Headers) -> 
    NewHeaders = [H,$:,V,$\r,$\n | Headers],
    build_headers(T, NewHeaders);
build_headers([], Headers) -> Headers.

-spec maybe_atom_to_list(string() | atom()) -> string().
maybe_atom_to_list(V) when is_atom(V) -> atom_to_list(V);
maybe_atom_to_list(V) when is_list(V) -> V.

-spec merge_headers(string(), boolean(), pos_integer() | undefined, http_headers()) -> http_headers().
merge_headers(HostHeaderValue, KeepAlive, ContentLength, UserHeaders) ->

    %Step 1. Strip any "connection" headers fro mthe UserHeaders
    H0 = [{HL, H, V} || {H,V} <- UserHeaders, 
        begin
            HL = string:to_lower(H),
            HL =/= "connection" andalso 
            HL =/= "content-length"
        end
    ],

    %Step 2. Check for a "host" UserHeader, if not present, use default
    H1 =
    case lists:keyfind("host", 1, H0) of
    false -> 
        [{"host", "Host", HostHeaderValue} | H0];
    _ ->
        H0
    end,

    %Step 3. Add the "connection" header if keepalive ...
    H2 =
    if KeepAlive ->
        [{"connection", "Connection", "keep-alive"} | H1];
    true ->
        H1
    end,

    %Step 4. Check for a "user-agent" UserHeader, if not present, use default
    H3 =
    case lists:keyfind("user-agent", 1, H2) of
    false -> 
        [{"user-agent", "User-Agent", "ELWHC/1.0"} | H2];
    _ ->
        H2
    end,

    %Step 5. Add the "content-length" header if defined ...
    H4 =
    if ContentLength =/= undefined ->
        [{"content-length", "Content-Length", integer_to_list(ContentLength)} | H3];
    true ->
        H3
    end,

    [{H, V} || {_, H,V} <- H4].

%EOF
