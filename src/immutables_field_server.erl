-module(immutables_field_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(DEFAULT_WIDTH, 40).
-define(DEFAULT_HEIGHT, 24).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([get/0, paint/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Returns current field state
-type field() :: term().
-spec get() -> {ok, field()}.
get() ->
    gen_server:call(?SERVER, get).

-spec paint(integer(), integer()) -> ok.
paint(X, Y) ->
    gen_server:cast(?SERVER, {paint, X, Y}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, immutables_field:new(?DEFAULT_WIDTH, ?DEFAULT_HEIGHT)}.

handle_call(get, _From, Field) ->
    {reply, {ok, Field}, Field};
handle_call(_Request, _From, Field) ->
    {noreply, ok, Field}.

handle_cast({paint, X, Y}, Field) ->
    {ok, Field1} = immutables_field:set(Field, X, Y),
    {noreply, Field1};
handle_cast(_Msg, Field) ->
  {noreply, Field}.

handle_info(_Info, Field) ->
  {noreply, Field}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

