-module(utils).
-include("kollekt.hrl").
-export([
  for/2,
  upsert_list/3, set_v/3,
  int_set_v/3, int_get_v/2, int_get_kv/2,
  random_str/2
  ]).

% for loop - needs a fun/1 (gets loop counter value)!
% example:
%   utils:for(3, fun(N)-> ...do something... end)
%
% if counter not needed in function:
%   utils:for(5, fun(_)-> ...do something... end)
%
for(N, Fun) when is_integer(N), is_function(Fun, 1) ->
  for({N, 0}, Fun);
for({N, _}, _) when is_integer(N), N < 1 ->
  ok;
for({N, LoopCount}, Fun) when is_integer(N), is_function(Fun, 1) ->
  Fun(LoopCount),
  for({N-1, LoopCount+1}, Fun).

% ETS integer helper

% updates or inserts list based Key-Value
upsert_list(Store, Key, Value) ->
  case ets:lookup(Store, Key) of
    [{Key, OldValue}] ->
      NewValue = [Value|OldValue],
      ets:insert(Store, { Key, NewValue });
    [] ->
      ets:insert(Store, { Key, Value })
  end.

% set a Key-Value pair
set_v(Store, Key, Value) ->
  ets:insert(Store, { Key, Value }).

% INTEGER setters and getters (values MUST be integers only)

% set a Key-Value pair
int_set_v(Store, Key, Value) when is_integer(Value) ->
  ets:insert(Store, { Key, Value }).

% returns Value
% default 0
int_get_v(Store, Key) ->
  case ets:lookup(Store, Key) of
    [{_Key, Value}] -> Value;
    []              -> 0
  end.

% returns {Key, Value}
% default {Key, 0}
int_get_kv(Store, Key) ->
  case ets:lookup(Store, Key) of
    [{K, V}] -> {K,   V};
    []       -> {Key, 0}
  end.

% random strings
random_str(Len, Chars) ->
  random:seed(now()),
  random_str(Len, Chars, random:seed(now())).

random_str(0,  _Chars, _State) -> [];
random_str(Len, Chars,  State) ->
  {Char, NewState} = random_char(Chars, State),
  [Char|random_str(Len-1, Chars, NewState)].

random_char(Chars, State) ->
  {Select, NewState} = random:uniform_s(tuple_size(Chars), State),
  {element(Select, Chars), NewState}.
