-module(nats_proto).

%% API exports
-export([connect_response/2, sub_request/2, pong_response/0]).

-spec connect_response(binary(), binary()) -> [binary()].
connect_response(User, Pass) when is_list(User) and is_list(Pass) ->
    connect_response(list_to_binary(User), list_to_binary(Pass));
connect_response(User, Pass) ->
    [<<"CONNECT ">>, jsx:encode(connect(User, Pass)), <<" \r\n">>].

-spec pong_response() -> [binary()].
pong_response() ->
    [<<"PONG \r\n">>].

-spec sub_request(binary(), binary()) -> [binary()].
sub_request(Subject, Sid) when is_list(Subject) and is_list(Sid) ->
    sub_request(list_to_binary(Subject), list_to_binary(Sid));
sub_request(Subject, Sid) ->
    [<<"SUB ">>, Subject, <<" ">>, Sid, <<" \r\n">>].

%%====================================================================
%% Internal functions
%%====================================================================

connect(undefined, undefined) ->
    connect();
connect(User, Pass) when is_binary(User) and is_binary(Pass) ->
    connect() ++ [{user, User}, {pass, Pass}].

connect() ->
    [
      {verbose, false},
      {pedantic, false},
      {ssl_required, false},
  %%    {auth_token, }
      {name, <<"">>},
      {lang, <<"erlang">>},
      {version, <<"0.0.1">>}
    ].
%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

connect_test() ->
    Response = connect(),
    Expected = [
      {verbose, false},
      {pedantic, false},
      {ssl_required, false},
      {name, <<"">>},
      {lang, <<"erlang">>},
      {version, <<"0.0.1">>}
    ],
    ?assertEqual(Expected, Response).

connect_with_user_and_pass_test() ->
    Response = connect(<<"testing">>, <<"password">>),
    Expected = [
      {verbose, false},
      {pedantic, false},
      {ssl_required, false},
      {name, <<"">>},
      {lang, <<"erlang">>},
      {version, <<"0.0.1">>},
      {user, <<"testing">>},
      {pass, <<"password">>}
    ],
    ?assertEqual(Expected, Response).

connect_response_test() ->
    Response = connect_response(),
    Expected = [<<"CONNECT ">>, <<"{\"verbose\":false,\"pedantic\":false,\"ssl_required\":false,\"name\":\"\",\"lang\":\"erlang\",\"version\":\"0.0.1\"}">>, <<" \r\n">>],
    ?assertEqual(Expected, Response).

sub_request_test() ->
    Response = sub_request("TESTING", "1"),
    Expected = [<<"SUB ">>, <<"TESTING">>, <<" ">>, <<"1">>, <<" \r\n">>],
    ?assertEqual(Expected, Response).

pong_response_test() ->
    Response = pong_response(),
    Expected = [<<"PONG \r\n">>],
    ?assertEqual(Expected, Response).
-endif.
