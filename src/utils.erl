-module(utils).
-export([for/2]).

%
% for-loop emulation
%
% N:     Number of times Fun should be called
% Fun: The high-level function to be called.
%          This function is expected to accept a single integer (the loop count)
%
for(N, Fun) when is_integer(N), is_function(Fun, 1) ->
  for({N, 0}, Fun)
;
for({N, _}, _) when is_integer(N), N < 1 ->
  ok
;
for({N, LoopCount}, Fun) when is_integer(N), is_function(Fun, 1) ->
  Fun(LoopCount),
  for({N-1, LoopCount+1}, Fun)
.
