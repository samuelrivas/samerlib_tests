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

%%% @doc This module tests FSM behaviour {@link sel_async_queue}
%%%
%%% Note that sequencing properties (i.e. that expected elements are read in the
%%% expected order) is tested by {@link sel_async_queue_proper}

-module(sel_async_queue_fsm_proper).

-behaviour(proper_fsm).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

%%% We are using the same internal state as learnerl, but that must be opaque
%%% for any other non-testing use of learnerl
-record(state, {queue :: sel_async_queue:async_queue()}).

%%% FSM Callbacks
-export([initial_state/0, initial_state_data/0, weight/3, precondition/4,
         postcondition/5, next_state_data/5]).

%%% FSM States
-export([no_queue/1, empty/1]).

%%% Transitions
-export([destroy/1]).

%%%===================================================================
%%% FSM Callbacks
%%%===================================================================

initial_state() -> no_queue.

initial_state_data() -> #state{queue = undefined}.

weight(_,_,_) -> 1.

precondition(_From, _To, _StateData, _Call) -> true.

postcondition(
  no_queue, no_queue, _StateData, {call, _, destroy, [Queue]}, Res) ->
    Res =:= {error, {invalid_queue, Queue}};
postcondition(
  _From, no_queue, StateData, _Call, _Res) ->
    is_queue_down(StateData#state.queue);
postcondition(
  _From, empty,_StateData,_Call, _Res) ->
    true;
postcondition(
  _From,_To,_StateData,_Call, _Res) ->
    false.

next_state_data(no_queue, empty, State, Res, _Call) ->
    State#state{queue = Res};
next_state_data(_, no_queue, State, _Res, _Call) ->
    State#state{queue = undefined};
next_state_data(_From, _Target, State, _Res, _Call) -> State.

%%%===================================================================
%%% States
%%%===================================================================
no_queue(#state{queue = Queue}) ->
    [
     {empty, {call, sel_async_queue, new, []}}
     , {no_queue, {call, ?MODULE, destroy, [Queue]}}
    ].

empty(#state{queue = Queue}) ->
    [{no_queue, {call, ?MODULE, destroy, [Queue]}}].

%%%===================================================================
%%% Transitions
%%%===================================================================
destroy(Queue) ->
    try sel_async_queue:destroy(Queue)
    catch Reason -> {error, Reason}
    end.

%%%===================================================================
%%% Properties
%%%===================================================================
prop_async_queue_fsm() ->
    ?FORALL(
       Cmds, proper_fsm:commands(?MODULE),
       ?TRAPEXIT(
          begin
              {H, S, R} = proper_fsm:run_commands(?MODULE, Cmds),
              ?WHENFAIL(report_error(Cmds, H, S, R), R =:= ok)
          end)).

%%%===================================================================
%%% Internals
%%%===================================================================
report_error(Cmds, H, S, R) ->
    report_history_and_states(Cmds, H, S),
    report_last_state(S),
    report_result(R),
    %% io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,R]).
    ok.

report_history_and_states(Cmds, History, _States) ->
    lists:foreach(
      fun({Cmd, H}) -> pretty_print_step(Cmd, H) end,
      sel_lists:cut_and_zip(Cmds, History)).

pretty_print_step(Command, {{StateName, StateData}, Result}) ->
    print_line(),
    pretty_print_state(StateName, StateData),
    pretty_print_command(Command, Result).

pretty_print_state(StateName, StateData) ->
    io:format(
      "State     : ~p~n"
      "State Data: ~p~n",
      [StateName, StateData]).

pretty_print_command({set, Var, {call, Mod, Fun, Args}}, Result) ->
    io:format("~p=~p:~p(", [Var,Mod, Fun]),
    io:format(
      "~s) -> ~p~n",
      [string:join([format("~p", [Arg]) || Arg <- Args], ","), Result]).

format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

print_line() -> io:format("~s~n", [lists:duplicate(70, $-)]).

report_last_state({StateName, StateData}) ->
    print_line(),
    pretty_print_state(StateName, StateData).

report_result(R) ->
    print_line(),
    io:format("~p~n", [R]).

is_queue_down(Queue) when is_pid(Queue) ->
    %% NOTE: This is cheating as the queue being a pid is exposing internal
    %% representation, don't do this in production code
    sel_process:wait_exit(Queue, 1000).
