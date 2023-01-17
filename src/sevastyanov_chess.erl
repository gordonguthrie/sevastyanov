-module(sevastyanov_chess).

-export([
			run/0,
			make_starting_board/0,
			draw_board/1
		]).

-define(EMPTYACCUMULATOR, []).

% ## API

run() ->
	Board = make_starting_board(),
	draw_board(Board).

make_starting_board() ->
    [
        {{black, rook},   a, 1},
        {{black, knight}, b, 1},
        {{black, bishop}, c, 1},
        {{black, queen},  d, 1},
        {{black, king},   e, 1},
        {{black, bishop}, f, 1},
        {{black, knight}, g, 1},
        {{black, rook},   h, 1},
        {{black, pawn},   a, 2},
        {{black, pawn},   b, 2},
        {{black, pawn},   c, 2},
        {{black, pawn},   d, 2},
        {{black, pawn},   e, 2},
        {{black, pawn},   f, 2},
        {{black, pawn},   g, 2},
        {{black, pawn},   h, 2},
        {{white, pawn},   a, 7},
        {{white, pawn},   b, 7},
        {{white, pawn},   c, 7},
        {{white, pawn},   d, 7},
        {{white, pawn},   e, 7},
        {{white, pawn},   f, 7},
        {{white, pawn},   g, 7},
        {{white, pawn},   h, 7},
        {{white, rook},   a, 8},
        {{white, knight}, b, 8},
        {{white, bishop}, c, 8},
        {{white, queen},  d, 8},
        {{white, king},   e, 8},
        {{white, bishop}, f, 8},
        {{white, knight}, g, 8},
        {{white, rook},   h, 8}
    ].

draw_board(Board) ->
	Blank = blank_board(),
	NewBoard = format_board(Board, ?EMPTYACCUMULATOR),
	io:format("NewBoard is ~p~n", [NewBoard]),
	B = apply_board(NewBoard, Blank, 0, []),
	io:format(B, []),
	ok.


% ## Private Functions

apply_board([], Rest, _N, Acc) -> lists:flatten(lists:reverse([Rest | Acc]));
apply_board([{Loc, Piece} | T], Board, N, Acc) ->
	io:format("Loc is ~p N is ~p~n", [Loc, N]),
	{NewAcc, [Dropped | Tail]} = lists:split(Loc - N - 1, Board),
	io:format("Dropped is ~p~n", [[Dropped]]),
	apply_board(T, Tail, Loc, [get_piece(Piece), NewAcc | Acc]).

format_board([], Acc) -> lists:sort(Acc);
format_board([{P, C, R} | T], Acc) ->
	NewC = shift_col(C),
	NewR = shift_row(R),
	Loc = make_loc(NewR, NewC),
	format_board(T, [{Loc, P} | Acc]).

make_loc(Row, Col) -> (Row * 30) + Col.

shift_row(N) -> (N * 3) - 1.

shift_col(a) -> 4;
shift_col(b) -> 7;
shift_col(c) -> 10;
shift_col(d) -> 13;
shift_col(e) -> 16;
shift_col(f) -> 19;
shift_col(g) -> 22;
shift_col(h) -> 25.

blank_board() ->
"  a   b  c  d  e  f  g  h   ~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"1 ░░░   ░░░   ░░░   ░░░    1~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"2    ░░░   ░░░   ░░░   ░░░ 2~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"3 ░░░   ░░░   ░░░   ░░░    3~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"4    ░░░   ░░░   ░░░   ░░░ 4~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"5 ░░░   ░░░   ░░░   ░░░    5~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"6    ░░░   ░░░   ░░░   ░░░ 6~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"7 ░░░   ░░░   ░░░   ░░░    7~n" ++
"  ░░░   ░░░   ░░░   ░░░     ~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"8    ░░░   ░░░   ░░░   ░░░ 8~n" ++
"     ░░░   ░░░   ░░░   ░░░  ~n" ++
"  a   b  c  d  e  f  g  h   ".

get_piece({black, rook})   -> "♜";
get_piece({black, knight}) -> "♞";
get_piece({black, bishop}) -> "♝";
get_piece({black, queen})  -> "♛";
get_piece({black, king})   -> "♚";
get_piece({black, pawn})   -> "♟︎";
get_piece({white, pawn})   -> "♙";
get_piece({white, rook})   -> "♖";
get_piece({white, knight}) -> "♘";
get_piece({white, bishop}) -> "♗";
get_piece({white, queen})  -> "♕";
get_piece({white, king})   -> "♔".


