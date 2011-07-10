% A proxy for socketio_listener gen_server
-module(immutables_socketio_server).
-export([start_link/0]).

start_link() ->
    {ok, Pid} = socketio_listener:start([
        {http_port, 8080},
        {default_http_handler, fun(Method, Path, Req) -> immutables_http_handler:handle(Method, Path, Req) end}
    ]),
    EventMgr = socketio_listener:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, immutables_socket_handler, []),
    {ok, Pid}.

