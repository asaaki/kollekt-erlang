% TEST EMITTER
-module(test_emitter).
-export([go/0, init/2]).
-include("kollekt.hrl").


go() ->
  spawn(fun() -> init("localhost", ?DEFAULT_PORT) end).

init(Host, Port) ->
  Args = init:get_plain_arguments(),
  case Args of
    [ArgHost, ArgPort] ->
      UseHost = ArgHost,
      {UsePort, _} = string:to_integer(ArgPort);
    _ ->
      UseHost = Host,
      UsePort = Port
  end,
  {ok, Socket} = gen_udp:open(0, [binary]),
  io:format("Test Emitter started. Target: ~p:~p~n" ,[UseHost, UsePort]),
  % we spawn 9 emitter processes
  utils:for(9, fun(_N)-> spawner(Socket, UseHost, UsePort) end),
  init_loop().

init_loop() -> init_loop().

loop(Socket, Host, Port) ->

  {A1,A2,A3} = now(),
  random:seed(A1, A2, A3),
  SessionSize = 1000000, % max amount of different sessions
  ValueSize   = 10000,   % max amount of different values
  Session = crypto:md5(list_to_binary(integer_to_list(random:uniform(SessionSize)+1))),
  Value   = crypto:md5(list_to_binary(integer_to_list(random:uniform(ValueSize)+1))),

  DataList = [Session,<<";">>,Value],
  BinData = list_to_binary(DataList),
  client(Socket, Host, Port, BinData),
  loop(Socket, Host, Port).

client(Socket, Host, Port, BinData) ->
  ok = gen_udp:send(Socket, Host, Port, BinData).


% helper

spawner(Socket, UseHost, UsePort) ->
  spawn(fun() -> loop(Socket, UseHost, UsePort) end).
