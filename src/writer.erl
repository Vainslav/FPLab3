-module(writer).

-include("point.hrl").
-export([start/2]).

start(Alg, Main) ->
    loop(Alg, Main).

loop(Alg, Main) ->
    receive
        #point{x = X, y = Y} ->
            io:format("~p: ~p ~p~n", [Alg, X, Y]);
        eof -> Main ! eof 
    end,
    loop(Alg, Main).