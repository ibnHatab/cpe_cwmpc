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

-record(state, {}).

-type common_reason() ::  'econn' | 'elogin' | 'eclosed' | term().

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
    gen_server:call(?MODULE, stop).


-spec connect(cwmp:url(),
	      Options :: [{atom(), term()}]) ->
		     {'ok', gen_session:session()} | {'error', common_reason()}.
connect(Host, Opts) when is_list(Opts) ->
    ?cwmprt("connect", [{host, Host}, {opts, Opts}]),
    try
	ConnectOptions = connect_options(Opts),
	gen_server:call(?SERVER, {connect, Host, ConnectOptions})
    catch
	throw:Error ->
	    ?cwmprt("connect - error", [{error, Error}]),
	    Error
    end.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({connect, _Host, _ConnectOptions}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
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

connect_test_() ->
    { setup,
      fun () ->
	      ok = lager:start(),
	      lager:set_loglevel(lager_console_backend, trace),
	      cpe_trace:enable(max, all),
	      cwmp:start_link()
      end,
      fun (_O)->
	      cwmp:stop(),
	      cpe_trace:disable()
      end,
      [ ?_test(begin
		   AcsUrl = "http://135.243.24.162:7003/cwmpWeb/CPEMgt",
		   Option = [{username, "mire"}, {password, "mire"}],
		   cwmp:connect(AcsUrl, Option),
		   ok
	       end)
      ]}.
    
-endif.
