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

run_commands(_Q, _Popper, []) ->
    true;
run_commands(Q, Popper, [{push, X} | T]) ->
    sel_async_queue:push(Q, X),
    run_commands(Q, Popper, T);
run_commands(Q, Popper, [pop | T]) ->
    Popper ! pop,
    run_commands(Q, Popper, T).

prop_sequencing() ->
    ?FORALL(
       Comms, commands(),
       ?TRAPEXIT(
          begin
              Q = sel_async_queue:new(),
              Popper = spawn_link(fun() -> popper(Q) end),
              run_commands(Q, Popper, Comms),
              Pushes = [X || {push, X} <- Comms],
              Pops = collect_pops(Popper),
              proper:equals(Pushes, Pops)
          end)).

collect_pops(Popper) ->
    Popper ! {get_elements, self()},
    receive {elements, Elements} -> Elements
    after 100 -> erlang:error(timeout)
    end.

popper(Q) ->
    popper(Q, []).

popper(Q, Acc) ->
    receive
        pop ->
            popper([sel_async_queue:pop(Q) | Acc]);
        {get_elements, Pid} ->
            Pid ! {elements, lists:reverse(Acc)}
    end.
