-module(light_sup).

-behaviour(supervisor).

%% API
-export([start/0, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
	spawn(fun() ->
	    	supervisor:start_link({local, ?MODULE}, ?MODULE, [])
		end).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	io:format("light_server pid=~w.~n", [self()]),
	Procs = [
		?CHILD(light, worker)
		],
    {ok, { {one_for_one, 5, 10}, Procs} }.

