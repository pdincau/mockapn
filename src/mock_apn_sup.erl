%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2013, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created : 21 Feb 2013 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(mock_apn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CERTIFICATE, "/path/to/certificate.pem").
-define(KEY, "/path/to/key.pem").
-define(PASSWORD, "password").

-define(MOCK_APN_PORT, 2195).

-define(SSLOPTIONS, [{certfile, ?CERTIFICATE},
                     {keyfile,  ?KEY},
		     {password, ?PASSWORD},
                     {mode, binary},
                     {packet, 0},
                     {backlog, 1000},
                     {active, true}]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ssl:start(),
    
    
    {ok, LSocket} = ssl:listen(?MOCK_APN_PORT, ?SSLOPTIONS),

    spawn_link(fun empty_listeners/0),

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 60,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 1000,
    Type = worker,

    AChild = {mock_apn_server, {mock_apn_server, start_link, [LSocket]},
	      Restart, Shutdown, Type, [mock_apn_server]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_socket() ->
    supervisor:start_child(?SERVER, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 2000)],
    ok.
