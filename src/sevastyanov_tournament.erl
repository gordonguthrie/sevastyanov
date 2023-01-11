%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% # vega_courtin is the gen server that handles a date
-module(sevastyanov_tournament).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% ## OTP API
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% API
-export([
         root/2
        ]).

-record(state, {}).

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

%

% Callbacks

init(Args) ->
    io:format("starting vega courtin for ~p~n", [Args]),
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

%% API

root(Route, Blah) ->
io:format("Route is ~p Blah is ~p", [Route, Blah]),
[<<"20 text/gemini\r\n# Your move\r\n">>].

%% Internal functions
