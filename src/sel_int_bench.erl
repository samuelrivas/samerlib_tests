%%% Copyright (c) 2012, Samuel Rivas <samuelrivas@gmail.com>
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * Neither the name the author nor the names of its contributors may
%%%       be used to endorse or promote products derived from this software
%%%       without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%% @doc Benchmarking functions for sel_int
-module(sel_int_bench).

%%%_* Exports ==========================================================
-export([sqrt/0, sqrt_bench/3]).

%%%_* Includes =========================================================

%%%_* Types ============================================================

%%%_* API ==============================================================

%% @doc Run a benchmark on several number size for sel_int:sqrt/1
sqrt() ->
  [sqrt_bench(1000000, 0, 32)
   , sqrt_bench(1000000, 33, 64)
   , sqrt_bench(100000, 65, 128)
   , sqrt_bench(100000, 129, 256)
  ].

sqrt_bench(Times, Min, Max) ->
  Self = self(),
  spawn_link(
    fun() ->
        random:seed(now()),
        Values = random_list(Times, Min, Max),
        {Microsecs, _} = timer:tc(fun() -> do_sqrt(Values) end),
        io:format(
          "~nsqrt on ~p to ~p bits numbers:~n"
          "~.2f sqrt per millisecond~n~.2f microsecs per sqrt~n",
          [Min, Max, Times*1000/Microsecs, Microsecs/Times]),
        Self ! done
    end),
  receive
    done -> ok
  end.

do_sqrt([]) -> ok;
do_sqrt([H|T]) ->
  sel_int:sqrt(H),
  do_sqrt(T).

%%%_* Private Functions ================================================

random_list(Values, MinBits, MaxBits) ->
  High = (1 bsl MaxBits) - 1,
  Low = (1 bsl MinBits),
  [random:uniform(High - Low) + Low - 1
   || _ <- lists:seq(1, Values)].

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
