-module(light).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).
-export([turn_on/1, turn_off/1, status/0, register/0]).

-define(SERVER, ?MODULE).

-record(state, {dev, key, light, task = [], client = []}).


%%------------------------------------------------------------------------------
%% external function
%%------------------------------------------------------------------------------
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
	gen_server:call(?SERVER, stop).

turn_on(Id) ->
	gen_server:call(?SERVER, {on, Id}).

turn_off(Id) ->
	gen_server:call(?SERVER, {off, Id}).

status() ->
	gen_server:call(?SERVER, {status}).	

register() ->
	gen_server:call(?SERVER, {register}).	

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),

	{ok, Cfg, _} = config:read_from_file("device.cfg", ["."]),
	{{DevMod, DevArg}, DevCfg} = Cfg,


	{ok, Dev} = DevMod:start_link(DevArg, DevCfg),

	State = #state{dev = Dev, key = [], light = []},
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

handle_call({on, Id}, From, State) ->
	#state{dev = Dev, task = Tasks} = State,
	dev_serial21:turn_on(Dev, Id),
	NewTasks = [{{on, Id}, From} | Tasks],
	{noreply, State#state{task = NewTasks}, 1000};

handle_call({off, Id}, From, State) ->
	#state{dev = Dev, task = Tasks} = State,
	dev_serial21:turn_off(Dev, Id),
	NewTasks = [{{off, Id}, From} | Tasks],
	{noreply, State#state{task = NewTasks}, 1000};

handle_call({status}, _, State) ->
	#state{light = Light} = State,
	{reply, {ok, {status, {light, Light}}}, State, 1000};

handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

%%------------------------------------------------------------------------------
%% handle_cast
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% handle_info
%%------------------------------------------------------------------------------
handle_info({light, LightStatus}, State) ->
	#state{task = Tasks, client = Clients, light = CurrentLight} = State,
	NewLight = update_status(CurrentLight, LightStatus),
	NewTasks = ack_task(NewLight, Tasks),
	notice_clients(NewLight, Clients),
	display_status(NewLight),
	{noreply, State#state{light = NewLight, task = NewTasks}};

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
status_is_match(StatusList, Id, Expect) ->
	R = case lists:keyfind(Id, 1, StatusList) of
		false -> false;
		{Id, Current} -> Current =:= Expect
	end,
	R.

batch_ack(Tasks, Result) ->
	lists:foreach(fun({{Type, Id}, From}) -> gen_server:reply(From, {Result, {Type, Id}}) end, Tasks).

ack_task(Lights, Tasks) ->
	{Finish, NotFinish} = lists:partition(fun({{Type, Id}, _From}) ->
		status_is_match(Lights, Id, Type) end, Tasks),
	batch_ack(Finish, ok),
	NotFinish.

clear_task(Tasks) ->
	batch_ack(Tasks, timeout),
	[].

notice_clients(Status, Clients) ->
	lists:foreach(fun({Pid, _}) -> Pid ! {light, Status} end, Clients).

display_status(Light) ->
	{On, Off} = lists:partition(fun({_Id, Status}) -> Status =:= on end, Light),
	io:format("~non : ~w ~noff: ~w ~n", [
		lists:map(fun({Id, _Status}) -> Id end, On),
		lists:map(fun({Id, _Status}) -> Id end, Off)]).

update_status(Current, New) ->
	lists:append(
		lists:filter(
			fun({Id, _}) -> not lists:keymember(Id, 1, New) end,
			Current),
		New).
