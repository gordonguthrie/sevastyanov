%%%-------------------------------------------------------------------
%% @doc universe top level supervisor.
%%
%% This is always empty - universe does nothing except start other
%% applications
%%
%% @end
%%%-------------------------------------------------------------------

-module(sevastyanov_games_sup).

-behaviour(supervisor).

-include("sevastyanov_game.hrl").

% ## OTP Exports

-export([start_link/0]).

-export([init/1]).

% ## API Exports

-export([
            start_game/1
        ]).

-define(SERVER, ?MODULE).

% # Public OTP API

% This is a normal Erlang OTP application.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% when you build your own app you will develop your own specialist
% servers or even supervisor/server subsystem trees and you will
% start them all from here

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

% ## API

start_game(Game) ->
    ChildSpec = make_child_spec(Game),
    {ok, Child} = supervisor:start_child(?MODULE, ChildSpec),
    io:format("in sevastyanov_games_sup start_game Child is ~p~n", [Child]),
    Child.

% ## Internal Functions

make_child_spec(Game) ->
    #game{id = Id} = Game,
    #{id       => Id,
      start    => {sevastyanov_game, start_link, [Game]},
      restart  => permanent,
      shutdown => 5000,
      type     => worker,
      modules  => [sevastyanov_game]}.
