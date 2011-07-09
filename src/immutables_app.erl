-module(immutables_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([handle_http/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    immutables_sup:start_link().

stop(_State) ->
    ok.

handle_http(Req) ->
    Req:ok("Hello world").
