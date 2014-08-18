-include("elwhc.hrl").

-define(parsed_url(S,H,P,RH,U,PQF), #parsed_url {scheme = S, host = H, port = P, raw_host = RH, user_info = U, path_query_frag = PQF}).

-record(parsed_url, {scheme, host, port, raw_host, user_info, path_query_frag}).
-type parsed_url() :: #parsed_url{}.

-record(elwhc_opts, {connect_timeout_ms = 120000, request_timeout_ms = 300000, keepalive_ms = 30000, keepalive = false, tcp_connect_options = [binary, {packet, http}, {active, false}, {reuseaddr, true}, {nodelay, true}, {keepalive, true}], ssl_options = []}).
-type elwhc_opts() :: #elwhc_opts{}.

-record(elwhc_request, {

      method
    , purl
    , body
    , headers
    , options = #elwhc_opts{}

    , t0 = os:timestamp()

    , socket

    , request_ttg_ms
    , request_ttg_t0

    , rsp_status
    , rsp_headers = []
    , rsp_body = <<>>

    , content_length = undefined

    , chunk_length_bytes = <<>>
    , chunk_bytes_to_go = 0
}).
-type elwhc_request() :: #elwhc_request{}.

-type elwhc_request_state() :: connect | connected | tx | rx | close.

%EOF
