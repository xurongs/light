-module(light_sup).

-behaviour(supervisor).

%% API
-export([start/2, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start(DevCfgFile, FuncList) ->
	spawn(fun() ->
	    	supervisor:start_link({local, ?MODULE}, ?MODULE, {DevCfgFile, FuncList})
		end).

start_link(DevCfgFile, FuncList) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {DevCfgFile, FuncList}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({DevCfgFile, FuncList}) ->
	AllProcs = [
		{light, {light, start_link, [DevCfgFile]}, permanent, 5000, worker, [light]},
		?CHILD(sche, worker),
		?CHILD(remote, worker),
		?CHILD(ix, worker)
		],
	Procs = lists:filter(fun({I, _, _, _, _, _}) -> lists:member(I, FuncList) end, AllProcs),
    {ok, { {one_for_one, 5, 10}, Procs} }.

