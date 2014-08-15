-module(remote).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).
-export([last/0]).
-export([switch/1, turn_on_a_while/2, dark_auto/2, sleep_auto/2]).

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

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	Uart = open_port({spawn, "./serial_forward /dev/ttySAC1"}, [stream]),
	link(Uart),
	Device = init_dev(),
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
init_dev() ->
	dict:from_list([
		{16#5344C0, [{?MODULE, switch,     [4]}]},
		{16#534430, [{?MODULE, switch,     [8]}]},
		{16#53450C, [{?MODULE, switch,     [18]}]},
		{16#D4B22E, [{?MODULE, dark_auto,  [13, 30]}]},
		{16#55E20A, [{?MODULE, sleep_auto, [14, 60]}]}
		]).

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
	lists:foreach(fun({Mod, Func, Arg}) -> apply(Mod, Func, Arg) end, lookup_proc(Scode, Device)).

light_status(Number) ->
	{ok, {status, {Light, _}}} = light:status(),
	case ((Light bsr Number) band 1) of
		0 -> off;
		1 -> on
	end.

switch(Number) ->
	light:case light_status(Number) of
		on ->
			turn_off;
		off ->
			turn_on
	end(Number).

dark() ->
	{Hour, _Minute, _Second} = time(),
	(Hour < 6) or (Hour >= 18).

dark_auto(Number, Seconds) ->
	case dark() of
		true ->
			turn_on_a_while(Number, Seconds);
		false ->
			void
	end.

sleep() ->
	{Hour, _Minute, _Second} = time(),
	(Hour < 6).

sleep_auto(Number, Seconds) ->
	case sleep() of
		true ->
			turn_on_a_while(Number, Seconds);
		false ->
			void
	end.

turn_on_a_while(Number, Seconds) ->
	case light_status(Number) of
		on ->
			void;
		off ->
			light:turn_on(Number),
			timer:apply_after(timer:seconds(Seconds), light, turn_off, [Number])
	end.
