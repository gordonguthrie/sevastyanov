%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% # Chess Tournament
-module(sevastyanov_tournament).

% This gen server holds the state the tournament:
%
% * a list of games waiting on person to join
% * all games currently being played
% ^

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% ## OTP API Exports
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% ## API calls Exports

-export([
         get_challenges/0
        ]).

% ## OTP Callback Exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {}).

% ## OTP API
% see: http://erlang.org/doc/man/gen_server.html#start_link-3
start_link_local() ->
    start_link_local(#{}).

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

% # API Functions
get_challenges() -> [[{challenge, "bingo"}], [{challenge, "bongo"}]].

% # OTP Callbacks

init(Args) ->
    io:format("starting sevastyanov_tournament for ~p~n", [Args]),
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    io:format("got request ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
