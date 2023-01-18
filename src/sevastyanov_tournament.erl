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

-define(TOURNAMENT, "tournament.txt").

-include("sevastyanov_game.hrl").

% ## OTP API Exports
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% ## API calls Exports

-export([
         get_challenges/0,
         get_public_games/0,
         start_game/3,
         view_game/2
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
         print_games/0,
         clear_state/0
        ]).

-record(state, {games = [], salt = [], dbroot = ""}).

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

get_public_games() -> gen_server:call(?MODULE, get_public_games).

start_game(Id, Name, Type) -> gen_server:call(?MODULE, {start_game, Name, Type, Id}).

view_game(GameId, Id) -> gen_server:call(?MODULE, {view_game, GameId, Id}).

% # Debug API Functions
print_games() -> gen_server:call(?MODULE, print_games).

clear_state() -> gen_server:call(?MODULE, clear_state).

% # OTP Callbacks

init(_Args) ->
    true = register(?MODULE, self()),
    {ok, Salt}   = application:get_env(sevastyanov, salt),
    {ok, DBRoot} = application:get_env(sevastyanov, dbroot),
    % create a state we can read from the db with
    State = #state{dbroot = DBRoot},
    S = read_tournament(State),
    % now update the read state with the environment variables
    % (which might have changed)
    {ok, S#state{salt = Salt, dbroot = DBRoot}}.

handle_call({view_game, GameId, Id}, _From, #state{games = Games} = State) ->
    Game = lists:keyfind(GameId, #game.id, Games),
    Reply = sevastyanov_game:view_game(Game#game.pid, Id),
    {reply, Reply, State};
handle_call(get_public_games, _From, #state{games = Games} = State) ->
    Reply = [{BinId, Name} || #game{id = BinId, name = Name, type = Type, status = Status} <- Games, Type == "public" andalso Status =/= waiting],
    {reply, Reply, State};
handle_call(get_challenges, _From, #state{games = Games} = State) ->
    Reply = [{BinId, Name} || #game{id = BinId, name = Name, type = Type, status = Status} <- Games, Type == "public" andalso Status == waiting],
    {reply, Reply, State};
handle_call(clear_state, _From, _State) ->
    io:format("clearing the state~n"),
    {ok, Salt}   = application:get_env(sevastyanov, salt),
    {ok, DBRoot} = application:get_env(sevastyanov, dbroot),
    {reply, ok, #state{salt = Salt, dbroot = DBRoot}};
handle_call(print_games, _From, #state{games = Games} = State) ->
    print_games(Games),
    {reply, ok, State};
handle_call({start_game, Name, Type, #{key := Key} = Id}, _From, State) ->
    #state{games = Games, salt = Salt} = State,
    BinId = make_id(Key, Salt),
    NewGame = #game{id = BinId, name = Name, white = Id, type = Type},
    PID = sevastyanov_games_sup:start_game(NewGame),
    NewState = State#state{games = [NewGame#game{pid = PID} | Games]},
    ok = write_tournament(NewState),
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

make_id(Key, Salt) ->
    Uniq = [integer_to_list(Key) ++ [get_timestamp()] ++ [Salt]],
    GameId = crypto:hash(md5, list_to_binary(Uniq)),
    SafeGameId = binary:encode_hex(GameId),
    binary_to_list(SafeGameId).

write_tournament(State) ->
    #state{dbroot = DBRoot} = State,
    PurgedState = purge_pids(State),
    ok = filelib:ensure_path(DBRoot),
    Filename = filename:absname_join(DBRoot, ?TOURNAMENT),
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, [PurgedState])),
    file:write_file(Filename, Text).

purge_pids(#state{games = Games} = State) ->
    NewGames = [X#game{pid = nul} || X <- Games],
    State#state{games = NewGames}.

read_tournament(State) ->
    #state{dbroot = DBRoot} = State,
    Filename = filename:absname_join(DBRoot, ?TOURNAMENT),
    case filelib:is_file(Filename) of
        true  -> {ok, [S]} = file:consult(Filename),
                 S;
        false -> #state{}
    end.

print_games([]) -> io:format("There are no games~n");
print_games(Games) -> [print_game(X) || X <- Games].

print_game(#game{id = Id, name = N, white = W, black = B, type = T, status = St}) ->
    io:format("Game ~p (~p)~nWhite ~p~nBlack ~p~n~p ~p~n", [N, Id, W, B, T, St]).

-define(MEGA, 1000000000000).
-define(SEC,  1000000).

-spec get_timestamp() -> pos_integer().
get_timestamp()->
    {Mega, Sec, Micro} = erlang:timestamp(),
    integer_to_list(?MEGA * Mega + ?SEC * Sec + Micro).
