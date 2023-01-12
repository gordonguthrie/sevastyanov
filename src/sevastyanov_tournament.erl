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
         get_challenges/0,
         start_game/3
        ]).

% ## OTP Callback Exports
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

% ## Debug Exports
-export([
         print_games/0
        ]).

-record(game, {id, name, first, second, type, status = waiting}).
-record(state, {games = [], salt = []}).

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
get_challenges() -> gen_server:call(?MODULE, get_challenges).

start_game(Id, Name, Type) -> gen_server:call(?MODULE, {start_game, Name, Type, Id}).

% # Debug API Functions
print_games() -> gen_server:call(?MODULE, print_games).

% # OTP Callbacks

init(_Args) ->
    true = register(?MODULE, self()),
    {ok, Salt} = application:get_env(sevastyanov, salt),
    {ok, #state{salt = Salt}}.

handle_call(get_challenges, _From, #state{games = Games} = State) ->
    Reply = [{BinId, Name} || #game{id = BinId, name = Name, type = Type, status = Status} <- Games, Type == "public" andalso Status == waiting],
    {reply, Reply, State};
handle_call(print_games, _From, #state{games = Games} = State) ->
    print_games(Games),
    {reply, ok, State};
handle_call({start_game, Name, Type, #{key := Key} = Id}, _From, State) ->
    #state{games = Games, salt = Salt} = State,
    Uniq = [integer_to_list(Key) ++ [get_timestamp()] ++ [Salt]],
    GameId = crypto:hash(md5, list_to_binary(Uniq)),
    SafeGameId = binary:encode_hex(GameId),
    BinId = binary_to_list(SafeGameId),
    NewGame = #game{id = BinId, name = Name, first = Id, type = Type},
    NewState = State#state{games = [NewGame | Games]},
    {reply, ok, NewState};
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

print_games([]) -> io:format("There are no games~n");
print_games(Games) -> [print_game(X) || X <- Games].

print_game(#game{id = Id, name = N, first = F, second = S, type = T, status = St}) ->
    io:format("Game ~p (~p)~nWhite ~p~nBlack ~p~n~p ~p~n", [N, Id, F, S, T, St]).


-define(MEGA, 1000000000000).
-define(SEC,  1000000).

-spec get_timestamp() -> pos_integer().
get_timestamp()->
    {Mega, Sec, Micro} = now(),
    integer_to_list(?MEGA * Mega + ?SEC * Sec + Micro).
