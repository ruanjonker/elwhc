-module(elwhc_util_tests).

-include("elwhc_private.hrl").
-include_lib("eunit/include/eunit.hrl").

url_encode_test() ->

    ?assertEqual("", ?uenc("")),

    ?assertEqual("", ?uenc(<<"">>)),

    ?assertEqual("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.", ?uenc("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.")),

    ?assertEqual("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.", ?uenc('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.')),

    ?assertEqual("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.", ?uenc(<<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.">>)),

    ?assertEqual("%0D%0A%3D%26%3F%3A%3B%3C%3E%40%23%22%21%24%25%28%29%2A%2B%2C-%20", ?uenc("\r\n=&?:;<>@#\"!$%()*+,- ")),

    ?assertEqual("%0D%0A%3D%26%3F%3A%3B%3C%3E%40%23%22%21%24%25%28%29%2A%2B%2C-%20", ?uenc(<<"\r\n=&?:;<>@#\"!$%()*+,- ">>)),

    ?assertEqual("12345", ?uenc(12345)),

    ok.

url_decode_test() ->

    ?assertEqual("", ?udec("")),

    ?assertEqual("", ?udec(<<"">>)),

    ?assertEqual("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.", ?udec("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.")),

    ?assertEqual("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.", ?udec(<<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.">>)),

    ?assertEqual("\r\n=&?:;<>@#\"!$%()*+,- ", ?udec("%0D%0A%3D%26%3F%3A%3B%3C%3E%40%23%22%21%24%25%28%29%2A%2B%2C-%20")),

    ?assertEqual("\r\n=&?:;<>@#\"!$%()*+,- ", ?udec(<<"%0D%0A%3D%26%3F%3A%3B%3C%3E%40%23%22%21%24%25%28%29%2A%2B%2C-%20">>)),

    ?assertEqual("  \r \n=&?:;<>@#\"!$%()*+,-  ", ?udec("++%0D+%0A%3D%26%3F%3A%3B%3C%3E%40%23%22%21%24%25%28%29%2A%2B%2C-%20+")),

    ?assertEqual("  \r \n=&?:;<>@#\"!$%()*+,-  ", ?udec(<<"++%0D+%0A%3D%26%3F%3A%3B%3C%3E%40%23%22%21%24%25%28%29%2A%2B%2C-%20+">>)),

    ok.

%EOF
