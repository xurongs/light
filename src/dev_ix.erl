-module(dev_ix).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, start/2, stop/1]).
-export([turn_on/2, turn_off/2, status/1]).
-export([listen_and_accept_proc/2]).
-export([diag/2]).

-define(SERVER, ?MODULE).

-record(state, {port, socket, parent, cfg}).


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

diag(Dev, Target) ->
	gen_server:call(Dev, {diag, Target}).

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init({Parent, Port, DevCfg}) ->
	self() ! listen,

	State = #state{port = Port, parent = Parent, cfg = DevCfg},
	{ok, State}.

%%------------------------------------------------------------------------------
%% handle_call
%%------------------------------------------------------------------------------
handle_call({diag, Target}, _, State) ->
	Result = case Target of
		state -> State;
		_ -> unknown
	end,
	{reply, Result, State, 1000};
	
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
handle_info({tcp, Socket, Bin}, State) ->
	#state{parent = Parent} = State,
	{status, Status} = binary_to_term(Bin),
	Parent ! Status,
	ok = inet:setopts(Socket, [{active, once}]),
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	self() ! listen,
	{noreply, State#state{socket = undefined}};

handle_info(listen, State) ->
	#state{port = Port} = State,
	listen_and_accept(Port),
	{noreply, State};

handle_info({tcp_accept, Socket}, State) ->
	io:format("The ix client is connected.~n", []),
	ok = inet:setopts(Socket, [{active, once}]),
	{noreply, State#state{socket = Socket}};

handle_info(_Info, State) ->
	{noreply, State}.

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
	case Socket of
		undefined ->
			ignore;
		_ ->
			gen_tcp:send(Socket, term_to_binary({Type, Id}))
	end.

listen_and_accept(Port) ->
	spawn_link(?MODULE, listen_and_accept_proc, [self(), Port]).

listen_and_accept_proc(Parent, Port) ->
	{ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, {active, false}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	ok = gen_tcp:controlling_process(Socket, Parent),
	Parent ! {tcp_accept, Socket},
	gen_tcp:close(Listen).

