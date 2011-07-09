-module(immutables_http_handler).
-export([handle_http/1]).

-include_lib("kernel/include/file.hrl").

handle_http(Req) ->
    handle(Req:get(method), Req:resource([urldecode]), Req).

handle('GET', [], Req) ->
    DocRoot = filename:join(code:priv_dir(immutables), "public"),
    Req:file(filename:join(DocRoot, "index.html"));
handle('GET', Path, Req) ->
    DocRoot = filename:join(code:priv_dir(immutables), "public"),
    FullPath = filename:join([DocRoot | Path]),
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
