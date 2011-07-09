-module(misultin_sup).
-export([start_link/0]).

start_link() ->
    MisultinConfig = [
	{port, 8080},
       	{loop, fun(Req) -> immutables_http_handler:handle_http(Req) end}
    ],
    misultin:start_link(MisultinConfig).
