-module(immutables_socket_handler).
-behaviour(gen_event).
-include_lib("./deps/socketio/include/socketio.hrl").
%% gen_event callbacks exports
-export([init/1,handle_event/2,handle_call/2,handle_info/2,terminate/2,code_change/3]).

-export([send_joined/3]).
%% internal API

send_state(Client, Field, Players) ->
    socketio_client:send(Client, #msg{content=[
                {<<"state">>, [
                        {<<"field">>, immutables_field:to_json_data(Field)},
                        {<<"users">>, [immutables_game_server:to_json_data(Player) || {_Name, Player} <- dict:to_list(Players) ]}
                    ]}], json=true}).

send_echo(Client, Content) ->
    socketio_client:send(Client, #msg{content=[{<<"echo">>, Content}], json=true}).

send_joined(Client, Name, Color) ->
    socketio_client:send(Client, #msg{content=[{<<"joined">>, [{<<"name">>, Name}, {<<"color">>, Color}]}], json=true}).

%% gen_event callbacks implementation

init([]) ->
    {ok, undefined}.

%% TODO: use fsm to implement client's states
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

handle_event({message, Client, #msg{ content = [{<<"login">>, Options}]}}, State) ->
    ParsedOptions = [{parse_atom(Key), Value} || {Key, Value} <- Options, Key == <<"name">> orelse Key == <<"color">> ],
    error_logger:info_msg("~p: login(~p)~n", [Client, ParsedOptions]),
    Name = proplists:get_value('name', ParsedOptions),
    Color = proplists:get_value('color', ParsedOptions),
    {ok, Players} = immutables_game_server:login(Client, Name, Color),
    {ok, Field} = immutables_field_server:get(),
    send_state(Client, Field, Players),
    {ok, State};

handle_event({message, Client, #msg{ content = Content }}, State) ->
    error_logger:info_msg("Message: ~p from ~p~n", [Content, Client]),
    send_echo(Client, Content),
    {ok, State};
handle_event(E, State) ->
    error_logger:error_msg("Unknown message ~p by ~p~n", [E, ?MODULE]),
    {ok, State}.

parse_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
