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

-behaviour(proper_fsm).

%%% We are using the same internal state as learnerl, but that must be opaque
%%% for any other non-testing use of learnerl
-record(state, {queue     :: sel_async_queue:async_queue(),
                collector :: pid(),
                pushed    :: [integer()]}).

%%% FSM Callbacks
-export([initial_state/0, initial_state_data/0, weight/3, precondition/4,
         postcondition/5, next_state_data/5]).

%%% FSM States
-export([uninitialised/1, initialised/1]).

%%% Transitions
-export([pop/2]).

%%% Internal processes
-export([collector/1]).

%%%===================================================================
%%% States
%%%===================================================================
uninitialised(_) ->
    [{initialised, {call, sel_async_queue, new, []}}].

initialised(#state{queue = Queue}) ->
    [{history,
      {call, sel_async_queue, push, [Queue, proper_types:integer()]}}
     , {history,
        {call, ?MODULE, pop, [{var, collector}, Queue]}}].

%%%===================================================================
%%% FSM Callbacks
%%%===================================================================
initial_state() -> uninitialised.

initial_state_data() -> #state{}.

weight(_,_,_) -> 1.

precondition(_,_,_,_) -> true.

postcondition(_From, _Target, _StateData, {call, _, push, _}, ok  ) -> true;
postcondition(
  _From, _Target, StateData, {call, _, pop, [Collector, _Queue]}, _Res) ->
    case StateData#state.pushed of
        [] ->
            %% Here we are blocked waiting for pushes, nothing to check
            true;
        L ->
            L =:= get_collected_elements(Collector)
    end;
postcondition(_From, _Target, _StateData, {call, _, new,  _}, _Res) -> true;

%% Fall through to false to avoid false positives due to matching errors
postcondition(_From, _Target, _StateData, _Call, _Res) ->
    false.

next_state_data(_From, _To, State, Queue, {call, _, new, _}) ->
    State#state{queue = Queue};

next_state_data(_From, _To, State, _Res, {call, _, push, What}) ->
    State#state{pushed = [What | State#state.pushed]};

next_state_data(_From, _Target, State, _Res, _Call) -> State.

%%%===================================================================
%%% Generators
%%%===================================================================

%%%===================================================================
%%% Transitions
%%%===================================================================
pop(Collector, Queue) -> Collector ! {pop, Queue}.

%%%===================================================================
%%% Properties
%%%===================================================================
prop_learnerl_fsm() ->
    ?FORALL(
       Cmds, proper_fsm:commands(?MODULE),
       ?TRAPEXIT(
          begin
              Collector = start_collector(),
              {H, S, R} =
                  proper_fsm:run_commands(
                    ?MODULE, Cmds, [{collector, Collector}]),
              Collector ! stop,
              ?WHENFAIL(report_error(H, S, R), R =:= ok)
          end)).

%%%===================================================================
%%% Internals
%%%===================================================================
report_error(H, S, R) ->
    io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,R]).

start_collector() ->
    spawn_link(fun collector/0).

get_collected_elements(Collector) ->
    Collector ! {get_collected, self()},
    receive
        {collected, L} -> L
    after 100 -> erlang:error(timeout)
    end.

collector() ->
    collector([]).

collector(L) ->
    receive
        {pop, Queue} ->
            Val = sel_async_queue:pop(Queue),
            ?MODULE:collector([Val | L]);
        {get_collected, Pid} ->
            Pid ! {collected, L};
        stop ->
            ok
    end.
