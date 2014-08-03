-module(light).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).
-export([turn_on/1, turn_off/1, status/0, register/0]).

-define(SERVER, ?MODULE).

-record(state, {uart, key, light, task = [], client = []}).


%%------------------------------------------------------------------------------
%% external function
%%------------------------------------------------------------------------------
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
	gen_server:call(?MODULE, stop).

turn_on(Number) ->
	gen_server:call(?MODULE, {on, Number}).

turn_off(Number) ->
	gen_server:call(?MODULE, {off, Number}).

status() ->
	gen_server:call(?MODULE, {status}).	

register() ->
	gen_server:call(?MODULE, {register}).	

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	Uart = open_port({spawn, "./serial_forward /dev/ttySAC2"}, [stream]),
	link(Uart),
	{{light, NewLights}, {key, NewKeys}} = init_data(Uart),
	State = #state{uart = Uart, key = NewKeys, light = NewLights},
	{ok, State}.

%%------------------------------------------------------------------------------
%% handle_call
%%------------------------------------------------------------------------------
handle_call({register}, From, State) ->
	#state{client = Clients} = State,
	{Pid, _} = From,
	case lists:keymember(Pid, 1, Clients) of
		false ->
			erlang:monitor(process, Pid),
			NewClients = [From | Clients];
		true -> NewClients = Clients
	end,
	{reply, {ok, {register, From}}, State#state{client = NewClients}, 1000};

handle_call({on, Number}, From, State) ->
	#state{uart = Uart, task = Tasks} = State,
	Uart ! {self(), {command, [2, Number, 255]}},
	NewTasks = [{{on, Number}, From} | Tasks],
	{noreply, State#state{task = NewTasks}, 1000};

handle_call({off, Number}, From, State) ->
	#state{uart = Uart, task = Tasks} = State,
	Uart ! {self(), {command, [3, Number, 255]}},
	NewTasks = [{{off, Number}, From} | Tasks],
	{noreply, State#state{task = NewTasks}, 1000};

handle_call({status}, _, State) ->
	#state{light = Lights, key = Keys} = State,
	{reply, {ok, {status, {Lights, Keys}}}, State, 1000};

handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

%%------------------------------------------------------------------------------
%% handle_cast
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% handle_info
%%------------------------------------------------------------------------------
handle_info({_Uart, {data, Data}}, State) ->
	#state{task = Tasks, client = Clients} = State,
	Status = {{light, NewLights}, {key, NewKeys}} = get_status(Data),
	NewTasks = ack_task(NewLights, Tasks),
	notice_clients(Status, Clients),
	display_status(Status),
	{noreply, State#state{key = NewKeys, light = NewLights, task = NewTasks}};

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
	#state{client = Clients} = State,
	NewClients = lists:keydelete(Pid, 1, Clients),
	{noreply, State#state{client = NewClients}};

handle_info(timeout, State) ->
	#state{task = Tasks} = State,
	NewTasks = clear_task(Tasks),
	{noreply, State#state{task = NewTasks}};

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
merge_value(V0, V1, V2) ->
	(V0 band 16#7f) bor ((V1 band 16#7f) bsl 7) bor ((V2 band 16#7f) bsl 14).
	
get_status([1, L0, L1, L2, K0, K1, K2, 16#ff]) ->
	{{light, merge_value(L0, L1, L2)}, {key, merge_value(K0, K1, K2)}}.
	
init_data(Uart) ->
	Uart ! {self(), {command, [16#ff, 16#ff]}},
	Uart ! {self(), {command, [1, 16#ff]}},
	receive
		{Uart, {data, Data}} ->
			get_status(Data)
	after 5000 ->
		timeout
	end.

on_off(OnOff) ->
	if
		OnOff =:= 0 -> off;
		true -> on
	end.

ack_task(Lights, [{{OnOff, Number}, From} = H | Cmds]) ->
	case OnOff =:= on_off(Lights band (1 bsl Number)) of
		true ->
			gen_server:reply(From, {ok, {OnOff, Number}}),
			ack_task(Lights, Cmds);
		false -> [H | ack_task(Lights, Cmds)]
	end;
ack_task(_, []) ->
	[].

notice_clients(Status, Clients) ->
	lists:foreach(fun({Pid, _}) -> Pid ! Status end, Clients).

clear_task(Tasks) ->
	lists:foreach(fun({{OnOff, Number}, From}) -> gen_server:reply(From, {timeout, {OnOff, Number}}) end, Tasks),
	[].

display_status({{light, Lights}, {key, Keys}}) ->
	io:format("~6w ~s~n", [title, "+ 1 2 3 4 5 6 7 8 9 + 1 2 3 4 5 6 7 8 9 +"]),
	display_status_list(light, bitmap_to_list(Lights)),
	display_status_list(switch, bitmap_to_list(Keys)).

display_status_list(Type, Lists) when is_list(Lists) ->
	io:format("~6w ~s~n", [Type,
		string:join(
			lists:map(fun(X) -> case X of false -> " "; true -> "*" end end, Lists)
			, " ")]).

bitmap_to_list(Value) ->
	lists:map(fun(X) -> ((Value bsr X) band 1) > 0 end,
		lists:seq(0, 21)).

