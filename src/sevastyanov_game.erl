%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(sevastyanov_game).

-include("sevastyanov_game.hrl").

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% API
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


% API Exports
-export([
          view_game/2
        ]).

% see: http://erlang.org/doc/man/gen_server.html#start_link-3
start_link_local() ->
    start_link_local(#game{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->
    start_link(#{}).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

% # Public API

view_game(GameId, Id) ->
    gen_server:call(GameId, {view_game, Id}).

% Callbacks

init(#game{} = Game) ->
    Board = sebastyanov_chess:make_starting_board(),
    {ok, Game#game{board = Board}}.

handle_call({view_game, Id}, _From, #game{white = Id} = State) ->
    {reply, State, State};
handle_call({view_game, Id}, _From, #game{status = waiting} = State) ->
    NewState = State#game{black = Id, status = playing},
    {reply, NewState, NewState};
handle_call({view_game, _Id}, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

-define(WHITE, #{colour = white, piece = nil}).
-define(BLACK, #{colour = black, piece = nil}).


