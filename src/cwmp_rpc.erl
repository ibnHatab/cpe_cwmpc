%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  This module implement HTTP protocol client part.
%%% @end
%%% Created :  7 Nov 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(cwmp_rpc).

-behaviour(gen_server).

-behaviour(gen_protocol).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% gen_protocol callbacks
-export([create_protocol/1, open/2, open_enable/2, open_done/2, demux/1]).


-define(SERVER, ?MODULE).

-record(state, {
	  registry %% ETS {SAP, gen_protocol:session()}
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_protocol callbacks
%%%===================================================================

create_protocol(Config) ->
    {ok, Config}.

open(InvokingProtocol, ParticipandSet) ->
    gen_server:call(?SERVER, {open, InvokingProtocol, ParticipandSet}).

open_enable(_InvokingProtocol, _ParticipandSet) ->
    {error, not_implemented}.

open_done(_InvokingProtocol, _ParticipandSet) ->
    {error, not_implemented}.

demux(_Message) ->
    {error, unknown_session_id}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{registry = ets:new(registry,[set])}}.

handle_call({open, InvokingProtocol, ParticipandSet}, _From, State) ->
    [{sap, Sap} | Rest] = ParticipandSet,
    {ok, SessionHttp} = cwmp_http:open(rpc, Rest),
    SessionRpc = try_register(InvokingProtocol, State, Sap, SessionHttp),
    {reply, {ok, SessionRpc}, State};

handle_call(stop, _From, S) ->
    {stop, normal, stopped, S}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_get_session(State,Sap) ->
    case ets:lookup(State#state.registry,Sap) of
	[] -> {error,noexists};
	[{_Sap,Session}] ->
	    Session
    end.

try_deregister(State,Sap) ->
    case ets:lookup(State#state.registry,Sap) of
	[] -> {error,noexists};
	[{_Sap,Session}] ->
	    exit(Session),
	    ets:delete(State#state.registry,Sap),
	    ok
    end.

try_register(cwmp, State, Sap, SessionHttp) ->
    case ets:lookup(State#state.registry,Sap) of
	[] ->
	    process_flag(trap_exit,true),
	    {ok,Pid} = cwmp_rpc_session:start_link(cwmp, SessionHttp),
	    ets:insert(State#state.registry, {Sap,Pid}),
	    ok;
	_ ->
	    {error,already_exists}
    end;
try_register(_Client, _State, _Sap,  _SessionHttp) ->
    {error, unknown_protocol}.

