-module(linear).

-include("point.hrl").

-export([start/2]).

-record(state, {points = [] :: list(point()), step :: number(), writer :: pid(), last_x_print = undefined :: number() | undefined}).
-type state() :: #state{}.

-spec start(pid(), number()) -> _.
start(Writer, Step) ->
    State = #state{writer = Writer, step = Step},
    loop(State).

-spec loop(state()) -> any().
loop(#state{points = []} = State) ->
    receive
      #point{} = P ->
        loop(State#state{points = [P]})
    end;
loop(#state{points = Points} = State) ->
    receive
      #point{} = P ->
        New_points = util:insert_sorted(Points, P),
        New_state = State#state{points = New_points},
        loop(try_print(New_state))
    end.

-spec try_print(state()) -> state().
try_print(#state{points = [_ | []]} = State) -> State;
try_print(#state{points = [F, #point{x = X_sec} = S | _], writer = Writer, last_x_print = undefined} = State) ->
    Y_print = calc_y(S, F, X_sec),
    Writer ! #point{x = X_sec, y = Y_print},
    try_print(State#state{last_x_print = X_sec});
try_print(#state{points = [#point{x = X_first} = F, S | _], step = Step, writer = Writer, last_x_print = Last_x_print} = State) when (Last_x_print + Step) < X_first ->
    New_x_print = Last_x_print + Step,
    Y_print = calc_y(F, S, New_x_print),
    Writer ! #point{x = New_x_print, y = Y_print},
    try_print(State#state{last_x_print = New_x_print});
try_print(State) -> State.

calc_y(#point{x = X0, y = Y0}, #point{x = X1, y = Y1}, X) ->
    ((Y1 - Y0) / (X1 - X0)) * (X - X0) + Y0.