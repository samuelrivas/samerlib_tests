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

%%% @doc functions to test sel_string module

-module(sel_string_proper).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------
prop_parse_hex() ->
    ?FORALL(
       {Binary, HexString}, bin_and_hex_str(),
       proper:equals(Binary, sel_string:parse_hex(HexString))).

prop_parse_invalid_hex() ->
    ?FORALL(
       {{_Bin, HexString}, HalfHex}, {bin_and_hex_str(), half_hex()},
       try
           sel_string:parse_hex(HalfHex ++ HexString),
           erlang:error(should_have_failed)
       catch
           invalid_hex_string -> true
       end).

prop_format_hex_roundtrip() ->
    ?FORALL(
       {Bin, Case}, {proper_types:binary(), letter_case()},
       proper:equals(
         Bin,
         sel_string:parse_hex(
           lists:flatten(
             sel_string:format_hex(Bin, Case))))).

prop_format_hex_roundtrip_default_is_upper() ->
    ?FORALL(
       Bin, proper_types:binary(),
       all_upper(lists:flatten(sel_string:format_hex(Bin)))).

prop_format_byte() ->
    ?FORALL(
       {Byte, Case}, {proper_types:byte(), letter_case()},
       begin
           Str = sel_string:format_byte(Byte, Case),
           proper:equals(<<Byte>>, sel_string:parse_hex(Str))
       end).

prop_format_byte_default_is_upper() ->
    ?FORALL(
       Byte, proper_types:byte(),
       all_upper(sel_string:format_byte(Byte))).

%%%-------------------------------------------------------------------
%%% Generators
%%%-------------------------------------------------------------------

%% Generates tuples like {Bin, Hex} where Bin is a binary and Hex the
%% hexadecimal representation of that binary. E.g.
%%
%% {<<0,255>>, "00FF"}
bin_and_hex_str() ->
    ?LET(
       {Bin, DeepStr}, bin_and_hex_deep_str(),
       {Bin, lists:flatten(DeepStr)}).

bin_and_hex_deep_str() ->
    ?LET(
       Bytes, proper_types:list(proper_types:byte()),
       {list_to_binary(Bytes), [byte_hex(B) || B <- Bytes]}).

%% Generates the hexadecimal representation of Byte, either in big letters or
%% small letters
byte_hex(Byte) ->
    ?LET(Case, letter_case(), sel_string:format_byte(Byte, Case)).

letter_case() -> proper_types:oneof([lower, upper]).

half_hex() ->
    [proper_types:elements(lists:seq($0, $9) ++ lists:seq($a, $f))].

all_upper(HexString) ->
    lists:all(
      fun(C) -> not lists:member(C, lists:seq($a, $f)) end,
      HexString).
