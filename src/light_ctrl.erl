-module(light_ctrl).
-export([start/1]).
%%% NOTE: lines with three %%% show code when frames are introduced

start(Browser) ->
	light:register(),
	send_status(Browser),
	loop(Browser).

loop(Browser) ->
	receive
	{Browser, {struct, [{on, LightIdx}]}} ->
		io:format("on ~w~n", [LightIdx]),
		light:turn_on(LightIdx),
		send_status(Browser),
		loop(Browser);
	{Browser, {struct, [{off, LightIdx}]}} ->
		io:format("off ~w~n", [LightIdx]),
		light:turn_off(LightIdx),
		send_status(Browser),
		loop(Browser);
	{Browser, {struct, Msg}} ->
		os:format("~w~n", [Msg]),
		loop(Browser);
	{{light, _Lights}, {key, _Keys}} ->
		send_status(Browser),
		loop(Browser)
	end.

send_status(Browser) ->
	{ok, {status, {Lights, _}}} = light:status(),
	io:format("status ~w~n", [Lights]),
	Browser ! [{cmd, cmd_state}, {state, Lights}].
