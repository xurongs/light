-module(ix).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).

-define(SERVER, ?MODULE).

-record(state, {host, port, socket = undefined}).


%%------------------------------------------------------------------------------
%% external function
%%------------------------------------------------------------------------------
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
	gen_server:call(?MODULE, stop).

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init([]) ->
	light:register(),

	{ok, Cfg, _} = config:read_from_file("ix.cfg", ["."]),
	{Host, Port} = Cfg,

	self() ! connect,
	timer:send_interval(timer:seconds(60), sync),

	State = #state{host = Host, port = Port},
	{ok, State}.

%%------------------------------------------------------------------------------
%% handle_call
%%------------------------------------------------------------------------------
handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

%%------------------------------------------------------------------------------
%% handle_cast
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% handle_info
%%------------------------------------------------------------------------------
handle_info({tcp, _Socket, Bin}, State) ->
	case binary_to_term(Bin) of
		{on, Id} ->
			light:turn_on(Id);
		{off, Id} ->
			light:turn_off(Id);
		_ ->
			undefined
	end,
	{noreply, State};

handle_info({light, _Status}, State) ->
	self() ! sync,
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	io:format("Disconnected from the ix server.~n"),
	self() ! connect,
	{noreply, State};

handle_info(sync, State) ->
	#state{socket = Socket} = State,
	case Socket of
		undefined ->
			ignore;
		_ ->
			ok = send_light_status(Socket)
	end,
	{noreply, State};

handle_info(connect, State) ->
	#state{host = Host, port = Port} = State,
	
	Result = gen_tcp:connect(Host, Port, [binary, {packet, 4}]),
	case Result of
		{ok, Socket} ->
			io:format("Connected to the ix server ~s:~w.~n", [Host, Port]),
			ok = send_light_status(Socket);
		_ ->
			{ok, _TRef} = timer:send_after(timer:seconds(5), connect),
			Socket = undefined
	end,
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
send_light_status(Socket) -> 
	{ok, Status} = light:status(),
	gen_tcp:send(Socket, term_to_binary(Status)).
