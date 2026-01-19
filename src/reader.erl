-module(reader).

-include("point.hrl").

-export([start/1]).

-spec start(atom()) -> any().
start(Alg) ->
    loop(Alg).

-spec loop(atom()) -> any().
loop(Alg) ->
    case io:get_line("") of
        eof ->
            Alg ! eof;
        Line ->
            Str = string:trim(Line),
            case Str of
                "" ->
                    loop(Alg);
                _ ->
                    case parse_pair(Str) of
                        {ok, {X, Y}} ->
                            Alg ! #point{x = X, y = Y},
                            loop(Alg);
                        {error, Reason} ->
                            io:format(stderr, "reader: cannot parse line '~s': ~p~n", [Line, Reason]),
                            loop(Alg)
                    end
            end
    end.

-spec parse_pair(string()) -> {ok | error, {number(), number()} | atom()}.
parse_pair(Line) ->
    Tokens = string:tokens(Line, " \t,;"),
    case Tokens of
        [Xs, Ys] ->
            try
                {ok, X} = util:parse_number(Xs),
                {ok, Y} = util:parse_number(Ys),
                {ok, {X, Y}}
            catch
                _:Err -> {error, Err}
            end;
        _ ->
            {error, bad_format}
    end.