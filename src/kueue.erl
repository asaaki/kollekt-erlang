% Queue
%
% needs:   bucket_broker.erl, stats.erl
% used_by: udp.erl

-module(kueue).
-behaviour(gen_server).

-export([
  start/1, stop/0, push/1,
  init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
  ]).

-define(DEFAULT_BUCKET_LIST_DELIMITER, <<"|||">>).
-define(DEFAULT_BUCKET_DATA_DELIMITER, <<";">>).

-record(state, {}).

start(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

push(Bin) -> gen_server:call(?MODULE, {do, Bin}).

init([]) ->
  io:format("queue started.~n"),
  {ok, #state{}}.

handle_call({do, Bin}, _From, State) ->
  spawn(fun() -> processPayload(Bin) end),
  stats:add(payload, bit_size(Bin)),
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% routines

processPayload(Payload) ->
  PayloadList = binary:split(Payload, ?DEFAULT_BUCKET_LIST_DELIMITER, [global]),
  lists:foreach(
    fun(I) ->
      spawn(fun() -> processBucketData(I) end)
    end,
    PayloadList).

processBucketData(Bin) ->
  [Bucket, Data] = binary:split(Bin, ?DEFAULT_BUCKET_DATA_DELIMITER),
  case bucket_broker:lookup(Bucket) of
    [{_BucketId, BucketPid}] ->
      bucket_broker:update(BucketPid, Data);
    [] ->
      bucket_broker:create(Bucket, Data)
  end,
  stats:incr({buckets, processed}).