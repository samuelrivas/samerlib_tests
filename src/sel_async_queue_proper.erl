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

%%% @doc Tests for sel_async_queue

-module(sel_async_queue_proper).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

commands() ->
    proper_types:list(
      proper_types:elements([{push, proper_types:integer()} , pop])).

run_commands(_Q, []) ->
    true;
run_commands(Q, [{push, X} | T]) ->
    %% we don't push in this process because it creates an asymmetry on
    %% tests. If we did that in practice sequences like [pop, {push, 0}] are
    %% very likely to succeed in an implementation that hangs forever if you try
    %% to pop an empty queue
    spawn_link(fun() -> sel_async_queue:push(Q, X) end),
    run_commands(Q, T);
run_commands(Q, [pop | T]) ->
    Self = self(),
    %% Shamefully using the length of the tail as sorting index because I'm too
    %% lazy to add a new argument to this recursion
    spawn_link(fun() -> Self ! {popped, sel_async_queue:pop(Q), length(T)} end),
    run_commands(Q, T).

prop_sequencing() ->
    ?FORALL(
       Comms, commands(),
       ?TRAPEXIT(
          begin
              run_commands(sel_async_queue:new(), Comms),
              Pushes = [X || {push, X} <- Comms],
              NumberOfPops = length([x || pop <- Comms]),
              Items = lists:min([length(Pushes), NumberOfPops]),
              Pops = collect_pops(Items),
              proper:equals(lists:sublist(Pushes, Items), Pops)
          end)).

collect_pops(N) ->
    Acc = collect_pops(N, []),
    [X || {X, _N} <- lists:reverse(lists:keysort(2, Acc))].

collect_pops(0, Acc) ->
    Acc;
collect_pops(N, Acc) ->
    receive {popped, X, Pos} -> collect_pops(N - 1, [{X, Pos} | Acc])
    after 100 -> erlang:error(timeout)
    end.
