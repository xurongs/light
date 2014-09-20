-module(light_ctrl).
-export([start/1]).
%%% NOTE: lines with three %%% show code when frames are introduced

start(Browser) ->
	light:register(),
	send_status(Browser),
	loop(Browser).

loop(Browser) ->
	receive
	{Browser, {struct, [{on, Light}]}} ->
		switch_light(Browser, turn_on, Light),
		loop(Browser);
	{Browser, {struct, [{off, Light}]}} ->
		switch_light(Browser, turn_off, Light),
		loop(Browser);
	{Browser, {struct, Msg}} ->
		os:format("~w~n", [Msg]),
		loop(Browser);
	{{light, _Lights}, {key, _Keys}} ->
		send_status(Browser),
		loop(Browser)
	end.

send_status(Browser) ->
	{ok, {status, {light, Light}}} = light:status(),
	Browser ! [{cmd, cmd_state}, {state, Light}].

switch_light(Browser, Turn, Light) ->
	Id = binary_to_atom(Light, latin1),
	io:format("~w ~w~n", [Turn, Id]),
	light:Turn(Id),
	send_status(Browser).

