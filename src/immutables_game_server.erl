-module(immutables_game_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([login/3, paint/3]).
-export([to_json_data/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(player, {client, name, color}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

login(Client, Name, Color) ->
    gen_server:call(?SERVER, {login, Client, Name, Color}).

paint(Client, X, Y) ->
    gen_server:cast(?SERVER, {paint, Client, X, Y}).

to_json_data(#player{name=Name, color=Color}) ->
    [{<<"name">>, Name}, {<<"color">>, Color}].
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, dict:new()}.

handle_call({login, Client, Name, Color}, _From, Players) ->
    case dict:find(Name, Players) of
        {ok, Player} ->
            NewPlayers = dict:store(Name, Player#player{client=Client}, Players),
            {reply, {ok, NewPlayers}, NewPlayers};
        _ -> 
            Player = #player{client=Client, 'name'=Name, color=Color},
            NewPlayers = dict:store(Name, Player, Players),
            joined(Player, Players),
            {reply, {ok, NewPlayers}, NewPlayers}
    end;

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

joined(#player{'name'=Name, color=Color}, Players) ->
    [immutables_socket_handler:send_joined(Client, Name, Color) || {_, #player{client=Client}} <- dict:to_list(Players) ].

handle_cast({paint, Client, X, Y}, State) ->
    OtherPlayers = [ Player || {_Name, #player{client=C} = Player} <- dict:to_list(State), Client =/= C],
    painted({X, Y}, OtherPlayers),
    {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

painted({X, Y}, L) ->
    [immutables_socket_handler:send_painted(Client, X, Y) || #player{client=Client} <- L ].

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

