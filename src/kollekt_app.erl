-module(kollekt_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 2323).

start(_StartType, _StartArgs) ->
  stats:start([]),
  kueue:start([]),
  bucket_broker:start([]),
  spawn(fun() -> udp:start(?DEFAULT_PORT) end),
  app_init().
  % or with supervisor:
  % kollekt_sup:start_link().

stop(_State) ->
  ok.

app_init() ->
  Pid = spawn(fun() -> app_loop() end),
  {ok, Pid}.

app_loop() ->
  app_loop().