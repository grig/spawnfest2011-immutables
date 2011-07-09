-module(immutables_field).
-compile(export_all).

-record(field, {width,height,cells=[]}).

new(Width, Height) ->
    #field{width=Width,height=Height,cells=new_cells(Width, Height)}.
    
new_cells(Width, _Height) ->
    lists:duplicate(Width, []).

to_json(#field{width=Width, height=Height, cells=Cells}) ->
    mochijson2:encode({struct, [{width, Width}, {height, Height}, {cells, Cells}]}).
