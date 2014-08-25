-module(elwhc_util).

-include("elwhc_private.hrl").

-export([
        url_encode/1,
        url_decode/1
        ]).

-spec url_encode(string() | binary() | atom() | integer()) -> string().
url_encode(Binary) when is_binary(Binary) ->
    url_encode(binary_to_list(Binary));

url_encode(Val) when is_atom(Val) ->
    url_encode(atom_to_list(Val));

url_encode(Val) when is_integer(Val) ->
    url_encode(integer_to_list(Val));

url_encode([H|T] = Str) when is_list(Str) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $- ->
            [H|url_encode(T)];
        true ->
            case erlang:integer_to_list(H, 16) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].


-spec url_decode(binary_or_string()) -> string().
url_decode(Binary)
when is_binary(Binary) ->
    url_decode(binary_to_list(Binary));

url_decode([$+ | T]) ->
    [$ | url_decode(T)];

url_decode([$%, H, L | T]) ->
    [erlang:list_to_integer([H,L], 16) | url_decode(T)];

url_decode([C | T]) ->
    [C | url_decode(T)];

url_decode([]) ->
    [].

%EOF
