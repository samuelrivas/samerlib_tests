%%% -*- mode: erlang -*-
%%%
%%% I mostly erased OTP release handling applications from my toolset in favour
%%% of very simplistic scrtips. Thus this file might be outdated and in need of
%%% slight changes in order to integrate this application in an OTPish release

{application, samerlib,
 [{description, "Tests for samerlib"},
  {vsn, "devel"},
  {modules, [yalog_test]},
  {registered, []},
  {applications, [kernel, stdlib, eunit]},
  {env, []}]}.
