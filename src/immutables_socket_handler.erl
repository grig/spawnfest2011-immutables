-module(immutables_socket_handler).
-behaviour(gen_event).
-include_lib("./deps/socketio/include/socketio.hrl").
%% gen_event callbacks exports
-export([init/1,handle_event/2,handle_call/2,handle_info/2,terminate/2,code_change/3]).

%% gen_event callbacks implementation

init([]) ->
    {ok, undefined}.

handle_event({client, Pid}, State) ->
    error_logger:info_msg("Connected: ~p~n", [Pid]),
    EventMgr = socketio_client:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE, []),
    {ok, State};
handle_event({disconnect, Pid}, State) ->
    error_logger:info_msg("Disconnected: ~p~n", [Pid]),
    {ok, State};
handle_event({message, Client, #msg{ content = [{<<"paint">>, [X, Y]}]}}, State) ->
    error_logger:info_msg("~p: paint(~p,~p)~n", [Client, X, Y]),
    immutables_field_server:paint(X, Y),
    {ok, State};
handle_event({message, Client, #msg{ content = Content } = Msg}, State) ->
    error_logger:info_msg("Message: ~p from ~p~n", [Content, Client]),
    socketio_client:send(Client, #msg{content=[{<<"echo">>, Content}], json=true}),
    {ok, State};
handle_event(E, State) ->
    error_logger:error_msg("Unknown message ~p by ~p~n", [E, ?MODULE]),
    {ok, State}.

handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
