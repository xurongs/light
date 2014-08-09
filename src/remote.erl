-module(remote).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).

-define(SERVER, ?MODULE).

-record(state, {uart}).


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
	process_flag(trap_exit, true),
	Uart = open_port({spawn, "./serial_forward /dev/ttySAC1"}, [stream]),
	link(Uart),
	State = #state{uart = Uart},
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
handle_info({_Uart, {data, Data}}, State) ->
	recv_signal(Data),
	{noreply, State};

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
recv_signal([0 | Data]) when length(Data) =:= 3 ->
	Scode = list2num(Data),
	io:format("~w~n", [Scode]);
recv_signal(_) ->
	void.

list2num(Data) ->
	lists:foldl(fun(X, Sum) -> X + Sum * 256 end, 0, Data).

proc_signal(_Scode) ->
	light:turn_off(4).
