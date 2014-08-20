
=======
elwhc
=====

Erlang Light-Weight HTTP Client

This is a work in progress...

https://www.ietf.org/rfc/rfc2616.txt

```bash

make run

```


```erlang

elwhc:request('GET', "https://www.google.co.za", <<>>, [], [{keepalive, false}]).

elwhc_sessions:list().

elwhc:request('GET', "https://www.google.co.za", <<>>, [], [{keepalive, true}]).

elwhc_sessions:list().



```


