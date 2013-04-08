%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2013, vlad
%%% @doc
%%%  Tests for RPC Session
%%% @end
%%% Created : 21 Mar 2013 by vlad <lib.aca55a@gmail.com>

-module(cwmp_rpc_session_tests).

-include_lib("xmerl/include/xmerl.hrl").
-include("cwmp/include/cwmp.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("femto_test/include/eunit_fsm.hrl").

-include("cpe_host/src/host_internal.hrl").

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
-define(SOAP_ENVELOPE, {'soapenv:Envelope',[],[]}).

rpc_sesson_test_() ->
    {foreach,
     fun ()  -> {ok, Pid} = start_link(cli, low), Pid end,
     fun (_Pid) -> stop() end,
     [
      %% Initialy (after start) in idle state
      ?_fsm_state(?SERVER, idle),
      ?_fsm_data(?SERVER, [cli, low]),
      %% Check RPC Inform scenario
      ?_fsm_test(?SERVER, "Master mode request",
		 [
		  {call, cwmp_rpc_session, push, [{rpc, #inform{}}], ok},
		  {state, is, master},
		  {call, cwmp_rpc_session, pop, [{soap, ?SOAP_ENVELOPE, false}], ok},
		  {state, is, idle}
		 ]),
      %% Check RPC Get scenario
      ?_fsm_test(?SERVER, "Slave mode request/response processing",
      		 [
      		  {state, is, idle},
		  {call, cwmp_rpc_session, push, [{rpc, #inform{}}], ok},
		  {state, is, master},
		  {call, cwmp_rpc_session, pop, [{soap, ?SOAP_ENVELOPE, true}], ok},
		  {state, is, hold},
		  %% receive message
		  {call, cwmp_rpc_session, pop, [{soap, ?SOAP_ENVELOPE, true}], ok},
		  {state, is, slave},
		  {call, cwmp_rpc_session, push, [{rpc, #get_rpc_methods_response{}}], ok},
		  {state, is, hold},
		  {call, cwmp_rpc_session, pop, [{soap, empty, false}], ok},
		  {state, is, idle},
      		  {call, erlang, self, [], self()}
      		 ])
     ]
    }.

%%--------------------------------------------------------------------
%% MOCK for RPC Client and HTTP Session
%%--------------------------------------------------------------------
client_mock_set() ->
    ok = meck:new(cwmp),
    meck:expect(cwmp, confirm, fun(_Data) -> 'ok' end),
    meck:expect(cwmp, indication, fun(_Data) -> 'ok' end).

client_mock_unset() ->
    true = meck:validate(cwmp),
    meck:unload(cwmp).


http_session_mock_set() ->
    ok = meck:new(cwmp_http_session),
    meck:expect(cwmp_http_session, push, fun(_Data) -> 'ok' end).

http_session_mock_unset() ->
    true = meck:validate(cwmp_http_session),
    meck:unload(cwmp_http_session).

rpc_session_mock_test_() ->
    {setup,
     fun() -> client_mock_set(), http_session_mock_set()  end,
     fun(_) -> client_mock_unset(), http_session_mock_unset() end,
     [
      ?_assertMatch(ok, cwmp:confirm({})),
      ?_assertMatch(ok, cwmp:indication({})),
      ?_assertMatch(ok, cwmp_http_session:push({}))
     ]
    }.

