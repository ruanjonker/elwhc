
-define(ELWHC_START_DEPS, [asn1, crypto, public_key,ssl]).

-type http_method() :: 'GET' | 'PUT' | 'POST' | 'DELETE' | 'HEAD' | 'OPTIONS'.

-type http_scheme() :: http | https.

-type http_host() :: string().

-type http_port() :: pos_integer().

-type http_host_port() :: {http_host(), http_port()}.

-type http_body() :: binary().

-type http_header() :: {string(), string()}.

-type http_headers() :: list(http_header()).

-type http_rsp_header() :: {string(), string(), string()}.

-type http_rsp_headers() :: list(http_rsp_header()).


-type http_option() ::    {connect_timeout_ms,  pos_integer()} 
                        | {request_timeout_ms,  pos_integer()} 
                        | {keepalive_ms,        pos_integer()} 
                        | {keepalive,           boolean()} 
                        | {max_sessions,        pos_integer()} 
                        | {max_requests_per_session,        pos_integer()} 
                        | {tcp_connect_options, list(term())} 
                        | {ssl_options,         list(term())}.

-type http_options() :: list(http_option()).

-type http_status_code() :: pos_integer(). %TODO: list actual status codes.

-type elwhc_request_result() :: {ok, http_status_code(), http_rsp_headers(), binary()} | {error, invalid_scheme} | {error, malformed_url} | {error, max_sessions} | {error, term()}.



%EOF
