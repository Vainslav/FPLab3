-module(newton).

-include("point.hrl").

-export([start/3]).

-record(state, {points = [] :: list(point()), step :: number(), writer :: pid(), last_x_print = undefined :: number() | undefined, n :: integer()}).
-type state() :: #state{}.

-spec start(pid(), number(), integer()) -> any().
start(Writer, Step, N) ->
    State = #state{writer = Writer, step = Step, n = N},
    loop(State).

-spec loop(state()) -> any().
loop(#state{points = []} = State) ->
    receive
      #point{} = P ->
        loop(State#state{points = [P]})
    end;
loop(#state{points = Points, n = N, writer = Writer} = State) ->
    receive
        eof -> 
            print_rest(State),
            Writer ! eof;
        #point{} = P ->
            New_points = util:insert_sorted(Points, P),
            New_state = State#state{points = New_points},
            Updated_state = if
                length(New_points) >= (N + 1)  ->
                    try_print(New_state);
                true -> New_state
            end,
            loop(Updated_state)
    end.



print_rest(#state{points = Points = [#point{x = X_first} | _T], writer = Writer, last_x_print = Last_x_print, n = N, step = Step} = State) when (Last_x_print + Step) < (X_first + 1) ->
    New_last_print = Last_x_print + Step,
    S_points = lists:sort(
                fun(#point{x = X}, #point{x = X2}) -> X2 > X end,
                Points
            ),
    Points_tuple = list_to_tuple(S_points),
    case util:bin_search(S_points, New_last_print) of
        {not_found, _} ->
            io:write("Ploxo");
        {found, I} -> 
            Writer ! element(I, Points_tuple);
        {between, I} ->
            Half_n = N div 2,
            StartIdx = max(1, I - Half_n + 1),
            EndIdx = min(tuple_size(Points_tuple), I + Half_n),
            
            IdxList = if 
                (EndIdx - StartIdx + 1) < N -> if
                       I < Half_n -> lists:seq(1, N);
                       I > tuple_size(Points_tuple) - Half_n -> 
                           lists:seq(tuple_size(Points_tuple) - N + 1, tuple_size(Points_tuple));
                       true -> lists:seq(I - Half_n + 1, I + Half_n)
                   end;
                true -> lists:seq(StartIdx, EndIdx)
            end,
            Selected_points = lists:map(
                fun(K) -> element(K, Points_tuple) end, 
                IdxList
            ),
            Sorted_points = lists:sort(
                fun(#point{x = X}, #point{x = X2}) -> X > X2 end,
                Selected_points
            ),
            case true of
                true ->
                    case newton_interpolation:interpolate(Sorted_points, New_last_print) of
                        {ok, YValue} ->
                            Result = #point{x = New_last_print, y = YValue},
                            Writer ! Result
                    end
            end
    end,
    print_rest(State#state{last_x_print = New_last_print});

print_rest(State) -> State.

-spec try_print(state()) -> state().
try_print(#state{points = Points, n = N} = State) when length(Points) < N ->
    State;
try_print(#state{points = Points, writer = Writer, last_x_print = undefined, n = _N} = State) ->
    P = #point{x = X} = lists:last(Points),
    Writer ! P,
    try_print(State#state{last_x_print = X});
try_print(#state{points = Points = [#point{x = X_first} | _T], step = Step, writer = Writer, last_x_print = Last_x_print, n = N} = State) when (Last_x_print + Step) < X_first ->
    New_last_print = Last_x_print + Step,
    S_points = lists:sort(
                fun(#point{x = X}, #point{x = X2}) -> X2 > X end,
                Points
            ),
    Points_tuple = list_to_tuple(S_points),
    case util:bin_search(S_points, New_last_print) of
        {not_found, _} ->
            io:write("Ploxo");
        {found, I} -> 
            Writer ! element(I, Points_tuple);
        {between, I} ->
            Half_n = N div 2,
            StartIdx = max(1, I - Half_n + 1),
            EndIdx = min(tuple_size(Points_tuple), I + Half_n),
            
            if 
                (EndIdx - StartIdx + 1) < N -> nil;
                true ->
                    IdxList = lists:seq(StartIdx, EndIdx),
                    Selected_points = lists:map(
                        fun(K) -> element(K, Points_tuple) end, 
                        IdxList
                    ),
                    Sorted_points = lists:sort(
                        fun(#point{x = X}, #point{x = X2}) -> X > X2 end,
                        Selected_points
                    ),
                    case true of
                        true ->
                            case newton_interpolation:interpolate(Sorted_points, New_last_print) of
                                {ok, YValue} ->
                                    Result = #point{x = New_last_print, y = YValue},
                                    Writer ! Result
                            end
                    end
            end
    end,
    try_print(State#state{last_x_print = New_last_print});
try_print(State) -> State.