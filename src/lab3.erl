-module(lab3).
-export([main/1]).

-record(opts, {algorithm = [linear] :: list(atom()), step = 1 :: integer()}).
-type opts() :: #opts{}.

-spec main(list(string())) -> any().
main(Args) ->
    #opts{algorithm = Alg, step = Step} = parse_args(Args),
    Writer = spawn(writer, start, [hd(Alg), self()]),
    Algorithm = case hd(Alg) of
        linear ->
            spawn(linear, start, [Writer, Step]);
        newton ->
            [_, N] = Alg,
            spawn(newton, start, [Writer, Step, N])
    end,
    _ = spawn(reader, start, [Algorithm]),
    loop().


-spec parse_args(list(string())) -> opts().
parse_args(Args) ->
    Opts = #opts{},
    parse_args(Args, Opts).

-spec parse_args(list(string()), opts()) -> opts().
parse_args([], Opts) ->
    Opts;

parse_args(["--linear" | Rest], Opts) ->
    parse_args(Rest, Opts#opts{algorithm = [linear]});

parse_args(["--newton", "-n", N | Rest], Opts) ->
    {Number, _} = string:to_integer(N),
    parse_args(Rest, Opts#opts{algorithm = [newton, Number]});

parse_args(["--step", N | Rest], Opts) ->
    {ok, Number} = util:parse_number(N),
    parse_args(Rest, Opts#opts{step = Number});

parse_args([_ | Rest], Opts) ->
    parse_args(Rest, Opts).

loop() ->
    receive
      eof ->
        nil
    end.