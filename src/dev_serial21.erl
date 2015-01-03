-module(dev_serial21).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, start/2, stop/1]).
-export([turn_on/2, turn_off/2, status/1]).
-export([diag/2]).

-define(SERVER, ?MODULE).

-record(state, {uart, parent, cfg, stat}).


%%------------------------------------------------------------------------------
%% external function
%%------------------------------------------------------------------------------
start(DevName, DevCfg) ->
	gen_server:start(?MODULE, {self(), DevName, DevCfg}, []).

start_link(DevName, DevCfg) ->
	gen_server:start_link(?MODULE, {self(), DevName, DevCfg}, []).

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
init({Parent, DevName, DevCfg}) ->
	process_flag(trap_exit, true),

	Uart = open_port({spawn, "./serial_forward "++DevName}, [stream]),
	link(Uart),
	activate_uart(Uart),
	State = #state{uart = Uart, parent = Parent, cfg = DevCfg, stat = {{rx, 0}, {tx, 0}}},
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
	NewState = switch_light_proc(State, off, Id), 
	{noreply, NewState};

handle_cast({on, Id}, State) -> 
	NewState = switch_light_proc(State, on, Id), 
	{noreply, NewState};

handle_cast(_Msg, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% handle_info
%%------------------------------------------------------------------------------
handle_info({_Uart, {data, Data}}, State) ->
	#state{parent = Parent, cfg = DevCfg, stat = {{rx, Rx}, Tx}} = State,
	Parent ! {light, get_light_status(Data, DevCfg)},
	{noreply, State#state{stat = {{rx, Rx + 1}, Tx}}};

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
activate_uart(Uart) ->
	Uart ! {self(), {command, [16#ff, 16#ff]}},
	Uart ! {self(), {command, [1, 16#ff]}}.

get_single_status(Status, Number) ->
	case (Status bsr Number) band 1 of
		0 -> off;
		1 -> on
	end.

convert_serial21_status(Status, DevCfg) ->
	lists:map(fun({Id, Num}) -> {Id, get_single_status(Status, Num)} end,
		DevCfg).

merge_value(V0, V1, V2) ->
	(V0 band 16#7f) bor ((V1 band 16#7f) bsl 7) bor ((V2 band 16#7f) bsl 14).

get_light_status([1, L0, L1, L2, _K0, _K1, _K2, 16#ff], DevCfg) ->
	convert_serial21_status(
		merge_value(L0, L1, L2),
		DevCfg).

type2cmd(Type) ->
	case Type of
		on -> 2;
		off -> 3
	end.

id2number(DevCfg, Id) ->
	{Id, Number} = lists:keyfind(Id, 1, DevCfg),
	Number.

switch_light_proc(State, Type, Id) ->
	#state{uart = Uart, cfg = DevCfg, stat = {Rx, {tx, Tx}}} = State,
	switch_light(Uart, type2cmd(Type), id2number(DevCfg, Id)),
	State#state{stat = {Rx, {tx, Tx + 1}}}.


switch_light(Uart, Cmd, Num) ->
	Uart ! {self(), {command, [Cmd, Num, 255]}}.

