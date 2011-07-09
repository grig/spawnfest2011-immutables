-module(immutables_http_handler).
-export([handle_http/1]).

-include_lib("kernel/include/file.hrl").

handle_http(Req) ->
    handle(Req:get(method), Req:resource([urldecode]), Req).

handle('GET', [], Req) ->
    Req:file("/Users/grig/Projects/spawnfest/immutables/priv/public/index.html");
handle('GET', Path, Req) ->
    FullPath = "/Users/grig/Projects/spawnfest/immutables/priv/public/" ++ filename:join(Path),
    case file:read_file_info(FullPath) of
	{ok, #file_info{type=regular}} ->
	    Req:file(FullPath);
	{ok, _} ->
	    Req:respond(404);
	{error, enoent} ->
	    handle_resource('GET', Path, Req)
    end;
handle(Method, Resource, Req) ->
    handle_resource(Method, Resource, Req).

handle_resource(_, _, Req) ->
    Req:ok("Hello, world!").
