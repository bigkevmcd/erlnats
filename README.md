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

To authenticate, pass user, pass as options.

```erlang
1> {ok, C} = nats:start_link([{user, "username"}, {pass, "password"}]).
{ok,<0.105.0>}
2> nats:subscribe(C, "testing.topic").
ok
```


Todo
----

* Generate a Sid (session ID) - currently uses a single value.
* Implement a callback mechanism for messages.
* Handle unexpected messages by failing.
* CT testing using a real Nats server.
