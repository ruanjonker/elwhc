
-type http_method() :: 'GET' | 'PUT' | 'POST' | 'DELETE' | 'HEAD' | 'OPTIONS'.

-type http_scheme() :: http | https.

-type http_host() :: string().

-type http_port() :: pos_integer().

-type http_host_port() :: {http_host(), http_port()}.

-type http_body() :: binary().

-type http_header() :: {string(), string()}.

-type http_headers() :: list(http_header()).

-type http_option() ::    {connect_timeout_ms,  pos_integer()} 
                        | {request_timeout_ms,  pos_integer()} 
                        | {keepalive_ms,        pos_integer()} 
                        | {keepalive,           boolean()} 
                        | {tcp_connect_options, list(term())} 
                        | {ssl_options,         list(term())}.

-type http_options() :: list(http_option()).

-type http_status_code() :: pos_integer(). %TODO: list actual status codes.



%EOF
