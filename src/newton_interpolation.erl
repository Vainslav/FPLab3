-module(newton_interpolation).
-export([interpolate/2]).

-include("point.hrl").

-spec interpolate(list(point()), number()) -> _.
interpolate(Points, TargetX) ->
    X_and_y = lists:map(fun(#point{x = X, y = Y}) -> {X, Y} end, Points),
    {XPoints, YPoints} = lists:unzip(X_and_y),
    case validate_input(XPoints, YPoints) of
        ok ->
            N = length(XPoints),
            DivDiff = calculate_divided_differences(XPoints, YPoints, N),
            Value = evaluate_polynomial(XPoints, DivDiff, TargetX, N),
            {ok, Value};
        Error ->
            Error
    end.
validate_input(XPoints, YPoints) ->
    case length(XPoints) == length(YPoints) of
        false -> {error, "Количество X и Y должно совпадать"};
        true ->
            case length(XPoints) < 2 of
                true -> {error, "Нужно хотя бы 2 точки для интерполяции"};
                false -> ok
            end
    end.

calculate_divided_differences(XPoints, YPoints, N) ->
    InitialTable = lists:duplicate(N, lists:duplicate(N, 0.0)),
    
    TableWithY = fill_first_column(InitialTable, YPoints, 0, N),
    
    calculate_diffs_recursive(TableWithY, XPoints, 1, N-1).

fill_first_column(Table, YPoints, Row, N) when Row < N ->
    UpdatedTable = set_element(Table, Row, 0, lists:nth(Row+1, YPoints)),
    fill_first_column(UpdatedTable, YPoints, Row+1, N);
fill_first_column(Table, _, _, _) -> Table.

calculate_diffs_recursive(Table, _, Column, N) when Column > N -> Table;
calculate_diffs_recursive(Table, XPoints, Column, N) ->
    NewTable = calculate_column(Table, XPoints, Column, 0, N-Column),
    calculate_diffs_recursive(NewTable, XPoints, Column+1, N).

calculate_column(Table, XPoints, Column, Row, MaxRow) when Row < MaxRow ->
    I = Row,
    J = Column,
    Xi = lists:nth(I+1, XPoints),
    Xij = lists:nth(I+J+1, XPoints),
    
    FNext = get_element(Table, I+1, J-1),
    FPrev = get_element(Table, I, J-1),
    
    Value = (FNext - FPrev) / (Xij - Xi),
    
    NewTable = set_element(Table, I, J, Value),
    calculate_column(NewTable, XPoints, Column, Row+1, MaxRow);
calculate_column(Table, _, _, _, _) -> Table.

evaluate_polynomial(XPoints, DivDiff, TargetX, N) ->
    evaluate_recursive(XPoints, DivDiff, TargetX, 0, 0.0, N).

evaluate_recursive(_, _, _, I, Sum, N) when I >= N -> Sum;
evaluate_recursive(XPoints, DivDiff, TargetX, I, Sum, N) ->
    Coef = get_element(DivDiff, 0, I),
    Product = calculate_product(XPoints, TargetX, I),
    NewSum = Sum + Coef * Product,
    evaluate_recursive(XPoints, DivDiff, TargetX, I+1, NewSum, N).

calculate_product(XPoints, TargetX, K) ->
    calculate_product_recursive(XPoints, TargetX, 0, K, 1.0).

calculate_product_recursive(_, _, I, K, Product) when I >= K -> Product;
calculate_product_recursive(XPoints, TargetX, I, K, Product) ->
    Xi = lists:nth(I+1, XPoints),
    NewProduct = Product * (TargetX - Xi),
    calculate_product_recursive(XPoints, TargetX, I+1, K, NewProduct).

get_element(Table, Row, Col) ->
    RowList = lists:nth(Row+1, Table),
    lists:nth(Col+1, RowList).

set_element(Table, Row, Col, Value) ->
    RowList = lists:nth(Row+1, Table),
    NewRowList = set_nth(RowList, Col+1, Value),
    set_nth(Table, Row+1, NewRowList).

set_nth(List, Pos, Value) ->
    set_nth_recursive(List, Pos, Value, 1, []).

set_nth_recursive([_|Rest], Pos, Value, Pos, Acc) ->
    lists:reverse([Value|Acc]) ++ Rest;
set_nth_recursive([Head|Rest], Pos, Value, Current, Acc) ->
    set_nth_recursive(Rest, Pos, Value, Current+1, [Head|Acc]).