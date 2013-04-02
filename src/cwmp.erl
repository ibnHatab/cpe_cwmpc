%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2013, vlad
%%% @doc
%%%  CWMP Client Interface
%%% @end
%%% Created : 25 Mar 2013 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(cwmp).

-behaviour(gen_server).

-include_lib("xmerl/include/xmerl.hrl").

-include("cwmp/include/cwmp.hrl").
-include("cwmpc_internal.hrl").

%% API
-export([start_link/0, stop/0]).
-export([connect/2]).

%% Constante used in internal state definition
-define(CONNECTION_TIMEOUT,  60*1000).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  session :: gen_protocol:session()
	 }).

-type common_reason() ::  'econn' | 'elogin' | 'eclosed' | term().

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

-spec connect(cwmp:url(), Options :: [{atom(), term()}]) ->
		     {'ok', gen_protocol:session()} | {'error', common_reason()}.
connect(Host, Opts) when is_list(Opts) ->
    ?cwmprt("connect", [{host, Host}, {opts, Opts}]),
    try
	URL = case xmerl_uri:parse(Host) of
		  {error, E} -> throw(E);
		  Res -> Res
	      end,
	ConnectOptions = connect_options(Opts),

	gen_server:call(?SERVER, {connect, URL, ConnectOptions})
    catch
	throw:Error ->
	    ?cwmprt("connect - error", [{error, Error}]),
	    {error, Error}
    end.

%% Request initiation
request(Session, Message) ->
    gen_server:call(?SERVER, {request, Session, Message}).

confirm(Req_Id, Message) ->
    gen_server:call(?SERVER, {confirm, Req_Id, Message}).

%% Request processing
indication(Session, Message) ->
    gen_server:call(?SERVER, {indication, Session, Message}).

response(Session, Message) ->
    gen_server:call(?SERVER, {response, Session, Message}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({connect, {_Proto,_Host,_Port,Path,Query} = Url, ConnectOptions}, _From, State) ->
    ParticipandSet = [{sap, Path ++ Query}, 	% Rpc peer identity
		      {url, Url}]		% HTTP Transport identity
	++ ConnectOptions,			% Extra option for RPC and transport
    SessionRpc = cwmp_rpc:open(cwmp, ParticipandSet),
    {reply, SessionRpc, State#state{session = SessionRpc}};

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

connect_options(Options) ->
    ?cwmprt("connect_options", [{options, Options}]),
    ValidateUser =
	fun(User) when is_list(User) -> true;
	   (_) -> false
	end,
    ValidatePasswd =
	fun(Passwd) when is_list(Passwd) -> true;
	   (_) -> false
	end,
    ValidateTimeout =
	fun(Timeout) when is_integer(Timeout) andalso (Timeout >= 0) -> true;
	   (_) -> false
	end,
    ValidOptions =
	[{username, ValidateUser,     true, elogin},
	 {password, ValidatePasswd,   true, elogin},
	 {timeout,  ValidateTimeout,  false, ?CONNECTION_TIMEOUT}],
    cpe_util:validate_options(Options, ValidOptions).


%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
