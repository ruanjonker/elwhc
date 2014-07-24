-include("elwhc.hrl").

-define(parsed_url(S,H,P,U,PQF), #parsed_url {scheme = S, host = H, port = P, user_info = U, path_query_frag = PQF}).

-record(parsed_url, {scheme, host, port, user_info, path_query_frag}).
-type parsed_url() :: #parsed_url{}.

-record(elwhc_opts, {connect_timeout_ms = 120000, request_timeout_ms = 300000, keepalive_ms = 30000, keepalive = false, tcp_connect_options = [{nodelay, true}], ssl_options = []}).
-type elwhc_opts() :: #elwhc_opts{}.

-record(elwhc_request, {method, purl, body, headers, options = #elwhc_opts{}, t0 = os:timestamp(), socket}).
-type elwhc_request() :: #elwhc_request{}.

-type elwhc_request_state() :: connect | connected | tx | rx | close.

%EOF
