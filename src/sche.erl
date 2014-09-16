-module(sche).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0, stop/0]).
-export([turn_on/2]).

-define(SERVER, ?MODULE).

-record(state, {task = dict:new()}).


%%------------------------------------------------------------------------------
%% external function
%%------------------------------------------------------------------------------
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
	gen_server:call(?MODULE, stop).

turn_on(Number, Seconds) ->
	gen_server:call(?MODULE, {{on, Number}, Seconds}).

%%------------------------------------------------------------------------------
%% init
%%------------------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	light:register(),
	{ok, #state{}}.

%%------------------------------------------------------------------------------
%% handle_call
%%------------------------------------------------------------------------------
handle_call({{on, Number}, Seconds}, _From, State) ->
	#state{task = Task} = State,
	{Result, NewTask} = turn_on_a_while(Number, Seconds, Task),
	{reply, Result, State#state{task = NewTask}};

handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

%%------------------------------------------------------------------------------
%% handle_cast
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% handle_info
%%------------------------------------------------------------------------------
handle_info({{light, Light}, {key, _Key}}, State) ->
	#state{task = Task} = State,
	NewTask = clear_task(Light, Task),
	{noreply, State#state{task = NewTask}};

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
clear_task(Light, Task) ->
	lists:foldl(
		fun(Key, Task0) ->
			{Number, OnOff} = Key,
			case light_status_2(Number, Light) of
				OnOff ->
					Task0;
				_ ->
					timer:cancel(dict:fetch(Key, Task)),
					dict:erase(Key, Task0)
			end
		end, Task, dict:fetch_keys(Task)).

light_status_2(Number, Light) ->
	case ((Light bsr Number) band 1) of
		0 -> off;
		1 -> on
	end.
light_status(Number) ->
	{ok, {status, {Light, _}}} = light:status(),
	light_status_2(Number, Light).

turn_on_a_while(Number, Seconds, Task) ->
	Key = {Number, on},
	case light_status(Number) of
		on ->
			case dict:is_key(Key, Task) of
				true ->
					{ok, cancel} = timer:cancel(dict:fetch(Key, Task)),
					{ok, TRef} = timer:apply_after(timer:seconds(Seconds), light, turn_off, [Number]),
					{ok, dict:store(Key, TRef, Task)};
				false ->
					{unmatch, Task}
			end;
		off ->
			light:turn_on(Number),
			{ok, TRef} = timer:apply_after(timer:seconds(Seconds), light, turn_off, [Number]),
			{ok, dict:store(Key, TRef, Task)}
	end.