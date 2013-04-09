%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2013, vlad
%%% @doc
%%%  http session test with ibrowse mock
%%% @end
%%% Created :  8 Apr 2013 by vlad <lib.aca55a@gmail.com>

-module(cwmp_http_session_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("femto_test/include/eunit_fsm.hrl").

-include("cpe_host/src/host_internal.hrl").

-import(cwmp_http_session, [start_link/2, stop/1,
			    push/1, pop/2]).

-define(HOST, "http://135.243.24.162:7003/cwmpWeb/CPEMgt").
-define(OPTION, [{username, "mire"}, {password, "mire"}, {timeout, 5000}]).


http_sesson_test_no() ->
    {foreach,
     fun ()  -> %%ensure_logging(),
		{ok, Pid} = start_link(xmerl_uri:parse(?HOST), ?OPTION),
		Pid
     end,
     fun (Pid) -> stop(Pid) end,
     [
      %% Initialy (after start) in idle state
      fun(Pid) -> ?_fsm_state(Pid, idle) end
     ]
    }.


ensure_logging() ->
    catch begin
	      [ok = application:start(A) || A <- [
						  sasl,
						  lager,
						  ibrowse
						 ]],
	      lager:set_loglevel(lager_console_backend, trace),
	      cpe_trace:enable(max, all)	      
	  end.


start_stop_test_() ->
    [
    {setup,
     fun ()  -> ensure_logging(),
		{ok, Pid} = start_link(xmerl_uri:parse(?HOST), ?OPTION),
		Pid end,
     fun (_) -> ok end,
      fun(Pid) -> ?_assertMatch(ok, stop(Pid)) end
    },
    {setup,
     fun ()  -> ensure_logging(),
		{ok, Pid} = start_link(xmerl_uri:parse(?HOST), ?OPTION),
		Pid end,
     fun (_) -> ok end,
      fun(Pid) -> ?_assertMatch(ok, stop(Pid)) end
    }].

