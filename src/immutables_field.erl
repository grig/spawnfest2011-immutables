-module(immutables_field).
-compile(export_all).

-record(field, {width,height,cells=[]}).

new(Width, Height) ->
    #field{width=Width,height=Height,cells=new_cells(Width, Height)}.
    
new_cells(Width, _Height) ->
    lists:duplicate(Width, lists:duplicate(_Height, 0)).

set(#field{cells=Columns} = Field, X, Y) ->
    Column = lists:nth(X + 1, Columns),
    Column1 = replace(Column, Y, 1),
    {ok, Field#field{cells=replace(Columns, X, Column1)}}.

% replace Nth element of list L with X
replace(L, N, X) ->
    {Prefix, [_|Tail]} = lists:split(N, L),
    Prefix ++ [X|Tail].

to_json(#field{width=Width, height=Height, cells=Cells}) ->
    mochijson2:encode({struct, [{width, Width}, {height, Height}, {cells, Cells}]}).
