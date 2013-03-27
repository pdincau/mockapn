%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2013, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created : 12 Jan 2013 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(mock_apn_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MAX_STANDBY, 120 * 60 * 1000).

-record(state, {buffer = <<>>, socket, tref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(LSocket) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LSocket]) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket = LSocket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(accept, #state{socket = LSocket} = State) ->
    ssl:setopts(LSocket, [{active, once}]),
    {ok, NewSocket} = ssl:transport_accept(LSocket),
    ok = ssl:ssl_accept(NewSocket),
    mock_apn_sup:start_socket(),

    TRef = erlang:send_after(?MAX_STANDBY, self(), disconnect),
    {noreply, State#state{socket = NewSocket, tref = TRef}};

handle_cast(parse, #state{socket = Socket, buffer = Buffer} = State) ->
    case Buffer of
        <<0:8, _BinTokenLength:16/big, _BinDeviceToken:256, PayloadLength:16/big, _BinPayload:PayloadLength/binary-unit:8, Rest/binary>> ->
	    io:format("~p~n", [_BinDeviceToken]),
            {noreply, State#state{buffer = Rest}};
        <<1:8, _Id:32/big, _Expiry:32/big, _BinTokenLength:16/big, _BinDeviceToken:256, PayloadLength:16/big, _BinPayload:PayloadLength/binary-unit:8, Rest/binary>> ->
	    DeviceToken = integer_to_list(_BinDeviceToken, 16),
	    io:format("~p~n", [DeviceToken]),
            {noreply, State#state{buffer = Rest}};
        _ ->
            {noreply, State#state{buffer = Buffer}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({ssl, Socket, Data}, #state{buffer = Buffer, tref = TRef} = State) ->
    erlang:cancel_timer(TRef),
    ssl:setopts(Socket, [{active, once}]),
    
    NewTRef = erlang:send_after(?MAX_STANDBY, self(), disconnect),
    gen_server:cast(self(), parse),
    {noreply, State#state{buffer = <<Buffer/binary, Data/binary>>, tref = NewTRef}};

handle_info({ssl_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(disconnect, #state{socket = Socket} = State) ->
    ssl:close(Socket), 
    {stop, normal, State};

handle_info(Info, State) ->
    error_logger:warning_msg("Received unexpected message: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
