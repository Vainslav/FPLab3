-module(util).

-include("point.hrl").

-export([insert_sorted/2, parse_number/1, bin_search/2]).

-spec insert_sorted(list(point()), point()) -> list(point()).
insert_sorted([], P) ->
    [P];
insert_sorted([#point{x = Xh} | _] = L, P = #point{x = X}) ->
    case Xh == X of
        true ->
            L;
        false when Xh > X ->
            L;
        false ->
            [P | L]
    end.

-spec parse_number(list()) -> {error | ok, number()}.
parse_number(N) ->
    {Number, _} = string:to_float(N),
    case Number of
        error -> 
            {M, I_desc} = string:to_integer(N),
            case M of
                error ->
                    {error, I_desc};
                M -> {ok, M}
            end;
        K -> {ok, K}
    end.


-spec bin_search([#point{}], number()) -> {found | not_found | between, integer()}.
bin_search([], _Key) -> {not_found, 1};
bin_search(Points, Key) when is_list(Points) -> 
    T = list_to_tuple(Points),
    bin_search(T, Key, 1, tuple_size(T)).

-spec bin_search(tuple(), number(), integer(), integer()) -> 
    {found | between | not_found, integer()}.
bin_search(_A, _Key, Lower, Upper) when Lower > Upper -> 
    {not_found, Lower};
bin_search(A, Key, Lower, Upper) ->
    Mid = Lower + (Upper - Lower) div 2,
    #point{x = Item_mid} = element(Mid, A),
    
    if
        Key == Item_mid -> 
            {found, Mid};
        Mid == Upper -> 
            if 
                Key > Item_mid -> {not_found, Upper + 1};
                true -> {between, Mid - 1}
            end;
        true ->
            #point{x = Item_next} = element(Mid + 1, A),
            if
                Key == Item_next -> {found, Mid + 1};
                (Key > Item_mid) and (Key < Item_next) -> {between, Mid};
                Key < Item_mid -> bin_search(A, Key, Lower, Mid - 1);
                Key > Item_mid -> bin_search(A, Key, Mid + 1, Upper)
            end
    end.