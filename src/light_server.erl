-module(light_server).
-export([start/0, start/1]).

%% This demo assumes that all your code
%% the code paths to ezwebframe and simple_demo are correct

start() ->
    start(8080).

start(Port) ->
    application:start(light),
    ezwebframe:start_link(fun dispatch/1, Port).

%% dispatch maps names in the HTML onto fixed paths 

dispatch(F) ->
    F1 = dispatch1(F),
    io:format("light_server::dispatch ~s => ~s~n",[F,F1]),
    F1.

dispatch1("/ezwebframe/" ++ F) ->
    Dir = dir(2, code:which(ezwebframe)) ++ "/priv/",
    Dir ++ F;
dispatch1("/" ++ F) ->
    Dir = dir(2,code:which(?MODULE)) ++ "/",
    Dir ++ F.

dir(0, F) -> F;
dir(K, F) -> dir(K-1, filename:dirname(F)).

