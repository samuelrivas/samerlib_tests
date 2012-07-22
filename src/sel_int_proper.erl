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

%%% @doc Proper tests for sel_int
-module(sel_int_proper).

%%%_* Exports ==========================================================
-export([]).

%%%_* Includes =========================================================

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

%%%_* Properties =======================================================

prop_extended_euclid() ->
    ?FORALL(
       {A, B}, pair(non_zero_int()),
       begin
           {X, Y} = sel_int:extended_euclid(A, B),
           D = X*A + Y*B,
           proper:conjunction(
             [{integer_x, is_integer(X)}
              , {integer_y, is_integer(Y)}
              , {divides_a, proper:equals(0, A rem D)}
              , {divides_b, proper:equals(0, B rem D)}
              , {no_greater_divisor, no_greater_divisor(A, B, D)}])
       end).

prop_integer_division() ->
    ?FORALL(
       {A, B}, {proper_types:integer(), non_zero_int()},
       begin
           {Q, R} = sel_int:int_div(A, B),
           proper:conjunction(
             [{result, proper:equals(A, Q * B + R)}
              , {integer_q, is_integer(Q)}
              , {integer_r, is_integer(R)}
              , {sign_q, Q*A*B >= 0}
              , {sign_r, R*A >= 0}])
       end).

%%%_* Generators =======================================================

non_zero_int() ->
    ?SUCHTHAT(N, proper_types:integer(), N /= 0).

pair(Gen) -> {Gen, Gen}.

%%%_* Private Functions ================================================

no_greater_divisor(A, B, Gcd) ->
    Candidates = lists:seq(abs(Gcd) + 1, erlang:min(abs(A), abs(B))),
    not lists:any(
          fun(X) -> (A rem X) =:= 0 andalso (B rem X) =:= 0 end,
          Candidates).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
