%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  RPC Session for bidirectional interaction with ACS
%%% @end
%%% Created :  7 Nov 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(cwmp_rpc_session).

-behaviour(gen_fsm).

-behaviour(gen_session).

-include("cwmpc_internal.hrl").

%% API
-export([start_link/2, stop/0]).

%% gen_session callbacks
-export([push/1, pop/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4,

	 idle/2, master/2, hold/2, slave/2,
	 idle/3, master/3, hold/3, slave/3
	]).

-define(SERVER, ?MODULE).

-record(state, {
	  client :: pid(),			% RPC client
	  lower  :: pid()			% lower session (HTTP)
	 }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(CwmpClient, HttpSession) ->
    ?cwmprt('start', [{client, CwmpClient}, {http_session, HttpSession}]),
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, {CwmpClient, HttpSession}, []).

stop() ->
    gen_fsm:sync_send_all_state_event(?SERVER,stop).


%%%===================================================================
%%% gen_session callbacks
%%%===================================================================
push(Message) ->
    ?cwmprt('rpc-push', [{msg, Message}]),
    gen_fsm:sync_send_event(?SERVER, {push, Message}).

pop(Message) ->
    ?cwmprt('rpc-pop', [{msg, Message}]),
    gen_fsm:sync_send_event(?SERVER, {pop, Message}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init({CwmpClient, HttpSession}) ->
    {ok, idle, #state{client = CwmpClient, lower = HttpSession}}.

%% NOTE: Bidirectional communication anchored on master and slave states
%% handle gen_fsm:send_event/2
idle(_Event, State) ->
    {next_state, idle, State}.

master(_Event, State) ->
    {next_state, master, State}.

hold(_Event, State) ->
    {next_state, hold, State}.

slave(_Event, State) ->
    {next_state, slave, State}.


%% handle gen_fsm:sync_send_event/[2,3]
idle({push, {rpc, _Data} = _Message}, _From, State) ->
    %% case cwmp_builder:build(Data) of

    %% 	end

    Reply = ok,
    {reply, Reply, master, State};
idle(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, idle, State}.

master({pop, {soap, _Data, false} = _Message}, _From, State) ->
    Reply = ok,
    {reply, Reply, idle, State};
master({pop, {soap, _Data, true} = _Message}, _From, State) ->
    Reply = ok,
    {reply, Reply, hold, State};
master(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, master, State}.

hold({pop, {soap, _Data, false} = _Message}, _From, State) ->
    Reply = ok,
    {reply, Reply, idle, State};
hold({pop, {soap, _Data, true} = _Message}, _From, State) ->
    Reply = ok,
    {reply, Reply, slave, State};
hold(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, hold, State}.

slave({push, {rpc, _Data} = _Message}, _From, State) ->
    Reply = ok,
    {reply, Reply, hold, State};
slave(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, slave, State}.


%% handle gen_fsm:send_all_state_event/[2,3]
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% handle gen_fsm:sync_send_all_state_event/[2,3]
handle_sync_event(stop, _From, _StateName, LoopData) ->
    {stop,normal,ok,LoopData};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% when receives any other message
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
