-module(remote).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).
-export([last/0]).
-export([manual/1]).
-export([switch/1, dark/0, apply_when/2, light_off/2]).

-define(SERVER, ?MODULE).

-record(state, {uart, last = 0, dev, recent = dict:new()}).


%%------------------------------------------------------------------------------
%% external function
%%------------------------------------------------------------------------------
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
	gen_server:call(?MODULE, stop).

last() ->
	{ok, {last, SCode}} = gen_server:call(?MODULE, last),
	io:format('~.16B~n', [SCode]).
	
manual(SCode) ->
	gen_server:cast(?MODULE, {scode, [0 | int_to_list(SCode, [])]}).

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init([]) ->
	Uart = open_port({spawn, "./serial_forward /dev/ttySAC1"}, [stream]),
	link(Uart),
	Device = load_dev(),
	State = #state{uart = Uart, dev = Device},
	{ok, State}.

%%------------------------------------------------------------------------------
%% handle_call
%%------------------------------------------------------------------------------
handle_call(last, _, State) ->
	#state{last = Last} = State,
	{reply, {ok, {last, Last}}, State};

handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

%%------------------------------------------------------------------------------
%% handle_cast
%%------------------------------------------------------------------------------
handle_cast({scode, SCode}, State) ->
	handle_info({self(), {data, SCode}}, State);

handle_cast(_Msg, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% handle_info
%%------------------------------------------------------------------------------
handle_info({_Uart, {data, Data}}, State) ->
	#state{dev = Device, recent = Recent} = State,
	{SCode, NewRecent} = recv_signal(Data, Device, Recent),
	{noreply, State#state{last = SCode, recent = NewRecent}};

handle_info({kill, SCode}, State) ->
	#state{recent = Recent} = State,
	NewRecent = recent_remove(SCode, Recent),
	{noreply, State#state{recent = NewRecent}};

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
load_dev() ->
	{ok, Cfg, _} = config:read_from_file("remote.cfg", ["."]),
	Rooms = lists:filtermap(fun({Cmd, {_Name, Room}}) -> case Cmd of room -> {true, Room}; _ -> false end end, Cfg),
	Devices = lists:map(
		fun({SCode, OpList}) -> {SCode,
			lists:map(
				fun(Op) ->
					case Op of
						{switch, Number} ->
							{switch, [Number]};
						{temporary, Number, Seconds} ->
							turn_on_when_dark(Rooms, Number, Seconds)
					end
				end,
				OpList)}
		end,
		lists:filtermap(fun({Cmd, SCode}) -> case Cmd of scode -> {true, SCode}; _ -> false end end, Cfg)),
	dict:from_list(Devices).
		
find_room(Rooms, Number) ->
	lists:flatten(
		lists:filter(
			fun(Lights) ->
				lists:any(
					fun(X) -> X =:= Number end,
					Lights)
				end,
			Rooms)).
		
turn_on_when_dark(Rooms, Number, Seconds) ->
	{apply_when, [[{dark, []}, {light_off, [find_room(Rooms, Number), [Number]]}], {sche, turn_on, [Number, Seconds]}]}.

recent_append(SCode, Recent) ->
	{ok, TRef} = timer:send_after(timer:seconds(2), {kill, SCode}),
	dict:append(SCode, TRef, Recent).

recent_remove(SCode, Recent) ->
	dict:erase(SCode, Recent).

recv_signal([0 | Data], Device, Recent) when length(Data) =:= 3 ->
	SCode = list2num(Data),
	NewRecent = case dict:is_key(SCode, Recent) of
		true ->
			Recent;
		false ->
			io:format("receive <~.16B>.~n", [SCode]),
			proc_signal(SCode, Device),
			recent_append(SCode, Recent)
	end,
	{SCode, NewRecent};
recv_signal(_, _, Recent) ->
	{-1, Recent}.

list2num(Data) ->
	lists:foldl(fun(X, Sum) -> X + Sum * 256 end, 0, Data).

lookup_proc(Scode, Device) ->
	case dict:is_key(Scode, Device) of
		true ->
			dict:fetch(Scode, Device);
		false ->
			[]
	end.	

proc_signal(Scode, Device) ->
	lists:foreach(fun(Proc) -> apply_1(Proc) end, lookup_proc(Scode, Device)).

light_status_2(Id, Lights) ->
	case lists:keyfind(Id, 1, Lights) of
		false -> unknown;
		{Id, Current} -> Current
	end.
	
all_light_status() ->
	{ok, {status, {light, Light}}} = light:status(),
	Light.

switch(Id) ->
	light:case light_status_2(Id, all_light_status()) of
		on ->
			turn_off;
		off ->
			turn_on;
		_ ->
			fun(_Id) -> unknown end
	end(Id).

apply_1({M, F, A}) ->
	apply(M, F, A);
apply_1({F, A}) ->
	apply(?MODULE, F, A).

apply_when(When, Apply) ->
	case lists:all(fun(MFA) -> apply_1(MFA) end, When) of
		true ->
			apply_1(Apply);
		false ->
			unmatch
	end.
	
light_off_3(Check, Except, Light) ->
	lists:all(
		fun(Number) -> 
			off == light_status_2(Number, Light)
		end,
		lists:subtract(Check, Except)).

light_off(Check, Except) ->
	light_off_3(Check, Except, all_light_status()).

dark() ->
	{Hour, _Minute, _Second} = time(),
	(Hour < 6) or (Hour >= 18).
	
int_to_list(0, List) ->
	List;
int_to_list(SCode, List) ->
	int_to_list(SCode div 256, [SCode rem 256 | List]).
