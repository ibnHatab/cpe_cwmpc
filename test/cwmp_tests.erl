%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2013, vlad
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2013 by vlad <lib.aca55a@gmail.com>

-module(cwmp_tests).

-include_lib("eunit/include/eunit.hrl").

-include("cpe_host/src/host_internal.hrl").


-include_lib("xmerl/include/xmerl.hrl").
-include("cwmp/include/cwmp.hrl").

cwmp_client_test_() ->
    { setup,
      fun () ->
	      [ok = application:start(A) || A <- [
						  sasl,
						  lager,
						  ibrowse
						 ]],
	      %% ok = lager:start(),
	      lager:set_loglevel(lager_console_backend, trace),
	      cpe_trace:enable(max, all),
	      {ok, _} = cwmp:start_link(),
	      {ok, _} = cwmp_rpc:start_link(),
	      {ok, _} = cwmp_http:start_link()		  
      end,
      fun (_O)->
	      cwmp:stop(),
	      cpe_trace:disable()
      end,
      [
       {"Try to connect to known ACS", fun acs_try_connect/0 }
      ]
    }.


acs_try_connect() ->
    AcsUrl = "http://135.243.24.162:7003/cwmpWeb/CPEMgt",
    Option = [{username, "mire"}, {password, "mire"}],
    {ok, Session} = cwmp:connect(AcsUrl, Option),
    ?DBG(Session),
    ok.


