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

%%% @doc functions to test sel_test module

-module(sel_test_proper).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

%%% Properties
-export([prop_test_in_dir/0, prop_parallel_test_in_dir/0]).

-behaviour(proper_statem).

%%% Callbacks
-export([command/1, initial_state/0, precondition/2, postcondition/3,
         next_state/3]).

%%% Internal exports
-export([test_in_dir/0]).

initial_state() ->
    none.

command(_S) -> {call, ?MODULE, test_in_dir, []}.

precondition(_S, _Call) -> true.

postcondition(_S, _Call, {_TmpDir, false}) ->
    error_dir_didnt_exist;
postcondition(_S, _Call, {TmpDir, true}) ->
    case filelib:is_file(TmpDir) of
        true -> {error_dir_still_exists, TmpDir};
        false -> true
    end.

next_state(S, _Res, _Call) -> S.

%% Test that the test fun is run in an existing directory, and that the
%% directory is removed after the test fun was run
prop_test_in_dir() ->
    ?FORALL(
       S, seed(),
       begin
           random:seed(S),
           {TempDir, Existed} =
               sel_test:test_in_dir(
                 fun(Dir) -> {Dir, filelib:is_dir(Dir)} end),
           proper:conjunction(
             [
              {  file_exists, Existed}
              , {file_is_cleand_up, not filelib:is_file(TempDir)}
             ])
       end).

%% Roughly the same as above, but simulating a parallel test suite
prop_parallel_test_in_dir() ->
    ?FORALL(
       {Seed, Testcase}, {seed(), proper_statem:parallel_commands(?MODULE)},
       begin
           random:seed(Seed),
           {_Sequential, _Parallel, Result} =
               proper_statem:run_parallel_commands(?MODULE, Testcase),
           Result =:= ok
       end).

%%%-------------------------------------------------------------------
%%% Generators
%%%-------------------------------------------------------------------
seed() ->
    {proper_types:int(), proper_types:int(), proper_types:int()}.

%%%-------------------------------------------------------------------
%%% helpers
%%%-------------------------------------------------------------------
test_in_dir() ->
    sel_test:test_in_dir(fun(Dir) -> {Dir, filelib:is_dir(Dir)} end).
