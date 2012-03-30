% Stats
%
% needs:   -
% used_by: bucket_broker.erl, bucket.erl, udp.erl

-module(stats).

-behaviour(gen_server).

-export([
  start/1, stop/0,
  incr/1, add/2, set/2, show/0,
  init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3,
  init_loop/0
  ]).

start(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

% insert() -> ok.
% upsert() -> ok.

incr(Key) ->
  gen_server:call(?MODULE, {incr_value, Key}).
add(Key, Value) ->
  gen_server:call(?MODULE, {add_value,  Key, Value}).
set(Key, Value) ->
  gen_server:call(?MODULE, {set_value,  Key, Value}).

show() ->
  gen_server:call(?MODULE, result).

init([]) ->
  io:format("stats started.~n"),
  init_loop(),
  {ok, ets:new(?MODULE,[set])}.

handle_call({incr_value, Key}, _From, Table) ->
  increment(Table, Key),
  Reply = ok,
  {reply, Reply, Table};

handle_call({add_value, Key, Value}, _From, Table) ->
  plus_v(Table, Key, Value),
  Reply = ok,
  {reply, Reply, Table};

handle_call({set_value, Key, Value}, _From, Table) ->
  set_v(Table, Key, Value),
  Reply = ok,
  {reply, Reply, Table};

handle_call(result, _From, Table) ->
  KeyList = [
    packets,
    payload,
    {buckets, created},
    {buckets, updated},
    {buckets, removed, timeout}
    ],
  Reply = {stats, [get_kv(Table, Key) || Key <- KeyList]},
  ets:delete_all_objects(Table),
  {reply, Reply, Table};

handle_call(_Request, _From, Table) ->
  Reply = ok,
  {reply, Reply, Table}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% helper

increment(Store, Key) ->
  plus_v(Store, Key, 1).

% decrement(Store, Key) ->
%   minus_v(Store, Key, 1).

set_v(Store, Key, Value) ->
  ets:insert(Store, { Key, Value }).

plus_v(Store, Key, Value) ->
  New = get_v(Store, Key) + Value,
  ets:insert(Store, { Key, New }).

% minus_v(Store, Key, Value) ->
%   New = get_v(Store, Key) - Value,
%   ets:insert(Store, { Key, New }).

get_v(Store, Key) ->
  case ets:lookup(Store, Key) of
    [{_Key, Value}] -> Value;
    []              -> 0
  end.

get_kv(Store, Key) ->
  case ets:lookup(Store, Key) of
    [{K, V}] -> {K,   V};
    []       -> {Key, 0}
  end.

init_loop() ->
  spawn(fun() -> output_loop() end).

output_loop() ->
  receive
    _Any -> output_loop()

  after 1000 ->
    {stats, CurrentResult} = show(),
    [
      {packets, Packets},
      {payload, Payload},
      {{buckets, created}, BucketsCreated},
      {{buckets, updated}, _BucketsUpdated},
      {{buckets, removed, timeout}, BucketsRemovedByTimeout}
    ] = CurrentResult,
    % OutList = [BucketsCreated, BucketsRemovedByTimeout, (BucketsCreated-BucketsRemovedByTimeout), Packets, (Payload/(8*1024*1024)), (Payload/(1000*1000))],
    io:format("=== | Buckets IN     ~7B/s | Packets           ~9B/s~n", [BucketsCreated,Packets]),
    io:format("    | Buckets OUT    ~7B/s | PayloadThroughput ~11.3f MiB/s~n", [BucketsRemovedByTimeout,(Payload/(8*1024*1024))]),
    io:format("    | Buckets IN/OUT ~7B/s |                   ~11.3f Mbit/s~n", [(BucketsCreated-BucketsRemovedByTimeout),(Payload/(1000*1000))]),
    output_loop()
  end.