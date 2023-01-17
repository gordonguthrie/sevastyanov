-module(sevastyanov_urls).

-define(NEWLINE, [10]).
-define(CRLF,    [13, 10]).

-export([
			make_link/3,
			make_action_link/4,
			make_redirect/1,
			make_input/1,
			make_unicode/1,
			make_challenges_menu/2,
			make_games_menu/1
		]).

make_link(Path, Extension, Text) ->
	URL   = string:join(Path ++ [Extension], "/"),
	Start = list_to_binary("=> " ++ "/" ++ URL ++ " "),
	Unicode = unicode:characters_to_binary(Text, utf8),
	Link = re:replace(<<Start/binary, Unicode/binary, "\n">>, ?NEWLINE, ?CRLF, [global]),
	Link.

make_action_link(Path, Extension, Id, Text) ->
	Ext = Path ++ [Extension],
	URL = string:join(Ext, "/"),
	Nonce = belka_router:get_nonce(URL, Id),
	make_link(Ext, Nonce, Text).

make_redirect(Path) ->
	URL = list_to_binary(Path),
    [<<"30 ", URL/binary, "\r\n">>].

make_input(Prompt) ->
	P = list_to_binary(Prompt),
    [<<"10 ", P/binary, "\r\n">>].

make_unicode(Text) -> unicode:characters_to_binary(Text, utf8).

make_challenges_menu([], _Id) -> "There are no challenges open";
make_challenges_menu(Challenges, Id) ->
	[make_action_link(["join", "game"], BinId, Id, "Join: " ++ Name) || {BinId, Name} <- Challenges].

make_games_menu([]) -> "There are no games being played";
make_games_menu(Games) ->
	[make_link(["game"], BinId, "Watch: " ++ Name) || {BinId, Name} <- Games].