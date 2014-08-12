-module(remote).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).
-export([last/0]).

-define(SERVER, ?MODULE).

-record(state, {uart, last = 0, dev}).


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
	gen_server:call(?MODULE, last).

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
	#state{dev = Device} = State,
	SCode = recv_signal(Data, Device),
	{noreply, State#state{last = SCode}};

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
init_dev() ->
	dict:from_list([
		{16#5344C0, [{light, turn_on, 4}]},
		{16#534430, [{light, turn_off, 4}]}
		]).

recv_signal([0 | Data], Device) when length(Data) =:= 3 ->
	SCode = list2num(Data),
	proc_signal(SCode, Device),
	io:format("~w~n", [SCode]),
	SCode;
recv_signal(_, _) ->
	-1.

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
	lists:foreach(fun({Mod, Func, Arg}) -> Mod:Func(Arg) end, lookup_proc(Scode, Device)).
