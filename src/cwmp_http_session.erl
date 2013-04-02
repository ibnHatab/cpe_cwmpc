%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  HTTP Session handler
%%% @end
%%% Created :  7 Nov 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(cwmp_http_session).

-behaviour(gen_fsm).

-behaviour(gen_session).

-include("cwmpc_internal.hrl").

%% API
-export([start_link/2]).

%% gen_session callbacks
-export([push/1, pop/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4,

	 idle/2, pending_response/2, pending_payload/2, receiving/2, 
	 idle/3, pending_response/3, pending_payload/3, receiving/3 
	]).

-define(SERVER, ?MODULE).

-record(state, {
	  host :: cwmp:url(),
	  username :: string(),
	  password :: string(),
	  timeout  :: timeout()
	 }).

-define(MAX_PIPELINE_SIZE, 1).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Host, Option) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, {Host, Option}, []).

stop() ->
    gen_fsm:sync_send_all_state_event(?SERVER,stop).


%%%===================================================================
%%% gen_session callbacks
%%%===================================================================
push(Message) ->
    ?cwmprt('http-push', [{msg, Message}]),
    gen_fsm:sync_send_event(?SERVER, {push, Message}).

pop({Message, Hold}) ->
    ?cwmprt('http-pop', [{msg, Message}, {hold, Hold}]),    
    gen_fsm:sync_send_event(?SERVER, {pop, Message, Hold}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init({{_Scheme, Host, Port, _Path, _Query} = Host, Option}) ->
    {value, Username} = lists:keysearch(username, 1, Option),
    {value, Password} = lists:keysearch(password, 1, Option),
    {value, Timeout} = lists:keysearch(timeout, 1, Option),

    ibrowse:set_max_pipeline_size(Host, Port, ?MAX_PIPELINE_SIZE),
        
    {ok, idle, #state{host = Host, username = Username,
		      password = Password, timeout = Timeout}}.

%% handle gen_fsm:send_event/2
idle(_Event, State) ->
    {next_state, idle, State}.

pending_response(_Event, State) ->
    {next_state, pending_response, State}.

pending_payload(_Event, State) ->
    {next_state, pending_payload, State}.

receiving(_Event, State) ->
    {next_state, receiving, State}.


%% handle gen_fsm:sync_send_event/[2,3]
idle({push, {soap, Data} = _Message}, _From, State) ->
    %% case request_stream(Data, State) of
	
    %% 	end

    Reply = ok,
    {reply, Reply, idle, State};
idle(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, idle, State}.

pending_response(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, pending_response, State}.

pending_payload(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, pending_payload, State}.

receiving(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, receiving, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%% handle gen_fsm:send_all_state_event/[2,3]
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% handle gen_fsm:sync_send_all_state_event/[2,3]
handle_sync_event(stop, _From, _StateName, LoopData) ->
    {stop,normal,ok,LoopData};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
%% handle_info({ibrowse_async_response_end, IbrowseRef}, StateName, State) ->
%%     ;

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
