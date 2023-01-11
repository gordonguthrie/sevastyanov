%%%-------------------------------------------------------------------
%% @doc universe top level supervisor.
%%
%% This is always empty - universe does nothing except start other
%% applications
%%
%% @end
%%%-------------------------------------------------------------------

-module(sevastyanov_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

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
    Games_sup  = {sevastyanov_games_sup, {sevastyanov_games_sup, start_link, []},
                  permanent, 5000, server, [sevastyanov_games_sup]},
    Tournament  = {sevastyanov_tournament, {sevastyanov_tournament, start_link, []},
                  permanent, 5000, worker, [sevastyanov_tournament]},

    ChildSpecs = [
                    Games_sup,
                    Tournament
                 ],
    {ok, {SupFlags, ChildSpecs}}.

% # There are no internal functions.