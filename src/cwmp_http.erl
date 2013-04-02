%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  This module implement HTTP protocol client part.
%%% @end
%%% Created :  7 Nov 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(cwmp_http).

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
	  registry %% ETS {URI, ibrowse handler}
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
    [{url, Host} | Rest] = ParticipandSet,
    SessionHttp = try_register(InvokingProtocol, State, Host, Rest),
    {reply, {ok, SessionHttp}, State};

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

try_get_session(State,Name) ->
    case ets:lookup(State#state.registry,Name) of
	[] -> {error,noexists};
	[{_Sap,Session}] ->
	    Session
    end.

try_deregister(State,Name) ->
    case ets:lookup(State#state.registry,Name) of
	[] -> {error,noexists};
	[{Sap,Session}] ->
	    exit(Session),
	    ets:delete(State#state.registry,Name),
	    ok
    end.

try_register(rpc, State, Host, Options) ->
    case ets:lookup(State#state.registry, Host) of
	[] ->
	    process_flag(trap_exit,true),
	    {ok,Pid} = cwmp_http_session:start_link(Host, Options),
	    ets:insert(State#state.registry, {Host, Pid}),
	    ok;
	_ ->
	    {error,already_exists}
    end;
try_register(_Client, _State, _Name, _SessionHttp) ->
    {error, unknown_protocol}.

