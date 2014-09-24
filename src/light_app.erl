-module(light_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	AppCfgFile = app_cfg_file(),
	{ok, {DevCfgFile, FuncList}, _} = config:read_from_file(AppCfgFile, ["."]),
    light_sup:start_link(DevCfgFile, FuncList).

stop(_State) ->
    ok.

app_cfg_file() ->
	case init:get_argument(app_cfg) of
		{ok, [[File]]} -> File;
		_ -> "app.cfg"
	end.
