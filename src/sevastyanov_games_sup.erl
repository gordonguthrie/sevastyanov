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

% ## OTP Exports

-export([start_link/0]).

-export([init/1]).

% ## API Exports

-export([
            get_games/0
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

% # API

get_games() ->
    [[{game, "ribble"}], [{game, "robble"}]].

% # There are no internal functions.