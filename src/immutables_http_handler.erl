-module(immutables_http_handler).
-export([handle_http/1]).

handle_http(Req) ->
    Req:ok("Hello world").
