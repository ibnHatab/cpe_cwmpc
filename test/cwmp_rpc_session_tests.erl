%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2013, vlad
%%% @doc
%%%  Tests for RPC Session
%%% @end
%%% Created : 21 Mar 2013 by vlad <lib.aca55a@gmail.com>

-module(cwmp_rpc_session_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("femto_test/include/eunit_fsm.hrl").

-include("cpe_host/src/host_internal.hrl").

-include_lib("xmerl/include/xmerl.hrl").
-include("cwmp/include/cwmp.hrl").


-import(cwmp_rpc_session, [start_link/2, stop/0,
			   push/1, pop/2]).

-define(SERVER, whereis(cwmp_rpc_session)).

self_test_() ->
    ?_assertEqual(self(), self()).

%%--------------------------------------------------------------------
%% @doc
%% Test RPC Session API via FSM inspection mechanism
%% @end
%%--------------------------------------------------------------------
rpc_sesson_test_() ->
    {foreach,
     fun ()  -> {ok, Pid} = start_link(cli, low), Pid end,
     fun (Pid) -> io:format(user, ">>  ~p stops ~p ~n", [self(), Pid]), stop() end,
     [
      %% Initialy (after start) in idle state
      %% ?_test(io:format(user, "2 >> ~p~n", [self()])),
      ?_fsm_state(?SERVER, idle),
      ?_fsm_data(?SERVER, [cli, low]),
      %% Check RPC Inform scenario
      ?_fsm_test(?SERVER, "Master mode request",
		 [
		  {call, cwmp_rpc_session, push, [#inform{}], ok},
		  {state, is, master},
		  {call, cwmp_rpc_session, pop, [{#inform_response{}, false}], ok},
		  {state, is, idle}
		 ]),
      %% Check RPC Get scenario
      ?_fsm_test(?SERVER, "Slave mode request/response processing",
		 [
		  {call, cwmp_rpc_session, push, [#inform{}], ok},
		  {state, is, master},
		  {call, cwmp_rpc_session, pop, [{#inform_response{}, true}], ok},
		  {state, is, hold},
		  {call, cwmp_rpc_session, pop, [{#get_rpc_methods{}, true}], ok},
		  {state, is, slave},
		  {call, cwmp_rpc_session, push, [#get_rpc_methods_response{}], ok},
		  {state, is, hold},
		  {call, cwmp_rpc_session, pop, [{{}, false}], ok},
		  {state, is, idle}
		 ])
     ]
    }.

%%--------------------------------------------------------------------
%% MOCK for RPC Client and HTTP Session
%%--------------------------------------------------------------------

