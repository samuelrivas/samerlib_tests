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
%%%
%%% Note that functions that generate random stuff (like {@link
%%% sel_test:test_in_dir/1}) are not automatically seeded, so tests can be
%%% repeated relaunching the test process.

-module(sel_test_eunit).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% test_in_dir tests
%%--------------------------------------------------------------------
dir_exists_test_() ->
    ?_assertMatch(
       true, sel_test:test_in_dir(fun(Dir) -> filelib:is_dir(Dir) end)).

lets_exceptions_trhough_test_() ->
    [?_assertThrow(
        ouch, sel_test:test_in_dir(fun(_) -> throw(ouch) end))
     ,?_assertExit(
         ouch, sel_test:test_in_dir(fun(_) -> exit(ouch) end))
     ,?_assertError(
         ouch, sel_test:test_in_dir(fun(_) -> erlang:error(ouch) end))].

dir_is_cleaned_up_test_() ->
    [?_assertMatch(
        false,
        begin
            Dir = sel_test:test_in_dir(fun(D) -> D end),
            filelib:is_file(Dir)
        end)
     ,?_assertMatch(
        true,
        begin
            Dir = sel_test:test_in_dir(false, fun(D) -> D end),
            Result = filelib:is_file(Dir),
            sel_file:delete_recursive(Dir),
            Result
        end)

     ,?_assertMatch(
         false,
         begin
             Dir = sel_test:test_in_dir(
                     fun(D) ->
                             file:write_file(
                               filename:join(D, "foo.txt"),
                               <<"foo">>)
                     end),
             filelib:is_file(Dir)
         end)
     ,?_assertMatch(
         false,
         begin
             Dir = try
                       sel_test:test_in_dir(
                         fun(D) -> erlang:error({ouch, D}) end),
                       throw(sould_have_failed)
                   catch
                       error:{ouch, D} -> D
                   end,
             filelib:is_file(Dir)
         end)].

%% Simply launching parallel tests in different process must trigger the
%% mechanism to avoid collisions, as the processes will all start with the same
%% seed, thus generating the same sequence of random directory names.
can_repeat_test_in_dir_test_() ->
    T = {spawn, ?_test(sel_test:test_in_dir(fun(_) -> true end))},
    {inparallel, lists:duplicate(10, T)}.
