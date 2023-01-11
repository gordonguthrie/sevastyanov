-module(sevastyanov_handlers).

-export([
			home/2,
			join_game/2,
			view_game/2,
			move/2,
			chat/2,
			history/2,
			request_game/2
		]).

%% API

home(Route, Blah) ->
	#{host := Host} = Route,
	Challenges = sevastyanov_tournament:get_challenges(),
	io:format("Challenges ~p~n", [Challenges]),
	Games = sevastyanov_games_sup:get_games(),
	io:format("Games ~p~n", [Games]),
	belka_templates:render(Host, "tournament", [{challenges, Challenges}, {games, Games}]).

join_game(Route, Blah) ->
	io:format("in join_game Route is ~p Blah is ~p~n", [Route, Blah]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

view_game(Route, Blah) ->
	io:format("in view_game Route is ~p Blah is ~p~n", [Route, Blah]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

move(Route, Blah) ->
	io:format("in move Route is ~p Blah is ~p~n", [Route, Blah]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

chat(Route, Blah) ->
	io:format("in chat Route is ~p Blah is ~p~n", [Route, Blah]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

history(Route, Blah) ->
	io:format("in history Route is ~p Blah is ~p~n", [Route, Blah]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].

request_game(Route, Blah) ->
	io:format("in request_game Route is ~p Blah is ~p~n", [Route, Blah]),
	[<<"20 text/gemini\r\n# Your move\r\n">>].
