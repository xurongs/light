-module(dev_ix).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, start/2, stop/1]).
-export([turn_on/2, turn_off/2, status/1]).

-define(SERVER, ?MODULE).

-record(state, {listen, socket, parent, cfg}).


%%------------------------------------------------------------------------------
%% external function
%%------------------------------------------------------------------------------
start(Port, DevCfg) ->
	gen_server:start(?MODULE, {self(), Port, DevCfg}, []).

start_link(Port, DevCfg) ->
	gen_server:start_link(?MODULE, {self(), Port, DevCfg}, []).

stop(Dev) -> 
	gen_server:call(Dev, stop).

turn_on(Dev, Id) ->
	gen_server:cast(Dev, {on, Id}).

turn_off(Dev, Id) ->
	gen_server:cast(Dev, {off, Id}).

status(Dev) ->
	gen_server:cast(Dev, {status}).

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init({Parent, Port, DevCfg}) ->
	process_flag(trap_exit, true),

	{ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
	self() ! {tcp_closed, undefined},

	State = #state{listen = Listen, parent = Parent, cfg = DevCfg},
	{ok, State}.

%%------------------------------------------------------------------------------
%% handle_call
%%------------------------------------------------------------------------------
handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

%%------------------------------------------------------------------------------
%% handle_cast
%%------------------------------------------------------------------------------
handle_cast({off, Id}, State) ->
	switch_light_proc(State, off, Id), 
	{noreply, State};

handle_cast({on, Id}, State) -> 
	switch_light_proc(State, on, Id), 
	{noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% handle_info
%%------------------------------------------------------------------------------
handle_info({tcp, _Socket, Bin}, State) ->
	#state{parent = Parent} = State,
	{status, Status} = binary_to_term(Bin),
	Parent ! Status,
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	#state{listen = Listen} = State,
	{ok, Socket} = gen_tcp:accept(Listen),
	io:format("The ix client is connected.~n"),
	{noreply, State#state{socket = Socket}};

handle_info(_Info, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% terminate
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%------------------------------------------------------------------------------
%% code_change
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
%%------------------------------------------------------------------------------
%% inner functions
%%------------------------------------------------------------------------------
switch_light_proc(State, Type, Id) ->
	#state{socket = Socket} = State,
	gen_tcp:send(Socket, term_to_binary({Type, Id})).

