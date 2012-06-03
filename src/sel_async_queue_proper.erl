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

-export([prop_sequencing/0]).

%%%--------------------------------------------------------------------
%%% Properties
%%%--------------------------------------------------------------------

%% @doc Test that the elements are popped in the same order as pushed
%%
%% This test creates two processes, one to push and other to pop, so that it can
%% check that the order is correct even when pops precede pushes
prop_sequencing() ->
    ?FORALL(
       Comms, commands(),
       ?TRAPEXIT(
          begin
              Q = sel_async_queue:new(),
              Popper = start_popper(Q),
              Pusher = start_pusher(Q),
              try
                  run_commands(Popper, Pusher, Comms),

                  Pushes = [X || {push, X} <- Comms],
                  Pops = collect_pops(Popper),

                  proper:equals(Pushes, Pops)
              after
                  sel_async_queue:destroy(Q),
                  stop_processes([Pusher, Popper])
              end
          end)).

%%%--------------------------------------------------------------------
%%% Generators
%%%--------------------------------------------------------------------
commands() ->
    ?LET(
       Pushes, pushes(),
       permutation(Pushes ++ lists:duplicate(length(Pushes), pop))).

pushes() -> proper_types:list({push, proper_types:integer()}).

%% TODO: this generator must be moved to a generic place. This is waiting for
%% story 30507175 to be completed
permutation([]) -> [];
permutation(L) ->
    ?LET(E, proper_types:elements(L), [E | permutation(lists:delete(E, L))]).

%%%--------------------------------------------------------------------
%%% Auxiliary functions
%%%--------------------------------------------------------------------
run_commands(Popper, Pusher, Commands) ->
    lists:foreach(
      fun(X) ->
              issue_command(Popper, Pusher, X),

              %% If we don't yield the scheduler will likely allow this process
              %% to run more reductions, so effectively it may fill the
              %% mailboxes of the pusher and the popper and then yielding to one
              %% of them, so we may end up testing only suspended or unsuspended
              %% queues depending on the scheduling algorithm
              erlang:yield() end,
      Commands).

issue_command(_Popper, Pusher, {push, X}) -> Pusher ! {push, X};
issue_command(Popper, _Pusher, pop)       -> Popper ! pop.

collect_pops(Popper) ->
    Popper ! {get_elements, self()},
    receive {elements, Elements} -> Elements
    after 100 -> erlang:error(timeout)
    end.

start_popper(Q) -> spawn_link(fun() -> popper(Q) end).

popper(Q) -> popper(Q, []).

popper(Q, Acc) ->
    receive
        pop ->
            Element = sel_async_queue:pop(Q),
            popper(Q, [Element | Acc]);

        {get_elements, Pid} ->
            Pid ! {elements, lists:reverse(Acc)}
    end.

%% The pusher is not strictly needed, but is easier to control the interleaving
%% of pops and pushes if both are issued from their own processes
start_pusher(Q) ->
    spawn_link(fun() -> pusher(Q) end).

pusher(Q) ->
    receive
        {push, Element} ->
            sel_async_queue:push(Q, Element),
            pusher(Q)
    end.

stop_processes(Pids) ->
    lists:foreach(
      fun(Pid) -> exit(Pid, kill) end,
      Pids).
