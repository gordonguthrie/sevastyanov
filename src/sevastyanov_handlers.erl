-module(sevastyanov_handlers).

-export([
			home/2,
			join_game/2,
			view_game/2,
			move/2,
			chat/2,
			history/2,
			start_game/2
		]).

-include("sevastyanov_game.hrl").

%% API

home(Route, _) ->
	#{host := Host, id := Id} = Route,
	F = make_tournament_footer(Host, Id),
	belka_templates:render(Host, "tournament", [{footer, F}]).

join_game(#{id := Id}, [{"gameid", GameId}]) ->
	_Game = sevastyanov_tournament:view_game(GameId, Id),
	URL = filename:join("/game", GameId),
	sevastyanov_urls:make_redirect(URL).

view_game(#{host := Host, id := Id} = Route, [{"gameid", GameId}]) ->
	io:format("in view_game Route is ~p~n", [Route]),
	Game = sevastyanov_tournament:view_game(GameId, Id),
	io:format("Game is ~p~n", [Game]),
	#game{name = N, white = W, black = B, board = Bd, history = _H, status = S} = Game,
	io:format("W is ~p~nB is ~p~n", [W, B]),
	WhiteName = get_name(W),
	BlackName = get_name(B),
	Board = 'Elixir.SevastyanovChess.Chess':draw_board(Bd),
	Args = [
			{name,      N},
			{status,    S},
			{blackname, BlackName},
			{board,     Board},
			{whitename, WhiteName},
			{user_menu, "menu here"},
			{details,   "details here"}
		    ],
	belka_templates:render(Host, "game_board", Args).

move(Route, Vals) ->
	io:format("in move Route is ~p Vals is ~p~n", [Route, Vals]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

chat(Route, Vals) ->
	io:format("in chat Route is ~p Vals is ~p~n", [Route, Vals]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

history(Route, Vals) ->
	io:format("in history Route is ~p Vals is ~p~n", [Route, Vals]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

start_game(#{querykvs := []}, _Vals) -> [sevastyanov_urls:make_input("Name your match")];
start_game(Route, [{"type", Type}]) when Type == "private" orelse
		                  		   	     			 Type == "public"  ->
  #{id := Id, querykvs := [{Name, true}]} = Route,
	ok = sevastyanov_tournament:start_game(Id, Name, Type),
	sevastyanov_urls:make_redirect("/");
start_game(Route, _Vals) -> belka_router:'51'(Route).

% ## Internal functions

get_name(#{name := Name}) -> Name;
get_name(_)               -> "awaiting challenger".

make_starts(Id) ->
	[
		sevastyanov_urls:make_action_link(["start", "game"], "private", Id, "Start a private match"),
		sevastyanov_urls:make_action_link(["start", "game"], "public",  Id, "Issue a public challenge")
	].

make_tournament_footer(Host, no_identity) ->
	Games = sevastyanov_games_sup:get_games(),
	G = sevastyanov_urls:make_games_menu(Games),
	belka_templates:render(Host, "tournament_logged_out_footer", [{games, G}]);
make_tournament_footer(Host, Id) ->
	% get the relevant data
	Challenges = sevastyanov_tournament:get_challenges(),
	Games = sevastyanov_tournament:get_public_games(),
	% process it
	C = sevastyanov_urls:make_challenges_menu(Challenges, Id),
	G = sevastyanov_urls:make_games_menu(Games),
	S = make_starts(Id),
	Args = [{challenges, C}, {games, G}, {starts, S}],
	belka_templates:render(Host, "tournament_logged_in_footer", Args).
