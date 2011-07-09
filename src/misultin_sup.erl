-module(misultin_sup).
-export([start_link/0]).

start_link() ->
  misultin:start_link([{port, 8080}, {loop, fun(Req) -> immutables_app:handle_http(Req) end}]).
