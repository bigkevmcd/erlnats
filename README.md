nats
=====

NOTE: experimental code, not for use.

A client library for Nats.io

Build
-----

    $ rebar3 compile

Usage
-----

```erlang
1> {ok, C} = nats:start_link().
{ok,<0.110.0>}
2> nats:subscribe(C, "testing.topic").
ok
received from topic <<"testing.topic">> message <<"hello\r\n">>
3>
```
