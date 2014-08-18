
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

[application:start(A) || A <- [asn1, crypto, public_key,ssl]].

{ok, Pid} = elwhc_request:start_link(),

elwhc:request('GET', "https://www.google.co.za", <<>>, [], [{keepalive, true}], Pid).

```


