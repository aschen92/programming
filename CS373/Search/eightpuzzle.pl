% eightpuzzle.pl

% formulation of 8-puzzle as a search problem

% representation: [blankX/blankY, oneX/oneY, twoX/twoY, ... nineX/nineY]
%   1/1 is lower-left

%------------------------------------------------------------
% Search problem definition

solution([2/2, 1/3, 2/3, 3/3, 3/2, 3/1, 2/1, 1/1, 1/2]).

succ([X/Y|Tiles0], up, [X/Y1|Tiles1]) :-
	Y < 3,
	Y1 is Y + 1,
	replace(Tiles0, X, Y1, X, Y, Tiles1).

succ([X/Y|Tiles0], down, [X/Y1|Tiles1]) :-
	Y > 1,
	Y1 is Y - 1,
	replace(Tiles0, X, Y1, X, Y, Tiles1).

succ([X/Y|Tiles0], left, [X1/Y|Tiles1]) :-
	X > 1,
	X1 is X - 1,
	replace(Tiles0, X1, Y, X, Y, Tiles1).

succ([X/Y|Tiles0], right, [X1/Y|Tiles1]) :-
	X < 3,
	X1 is X + 1,
	replace(Tiles0, X1, Y, X, Y, Tiles1).

replace([Targx/Targy|T], Targx, Targy, Newx, Newy, [Newx/Newy|T]) :- !.
replace([H|T], Targx, Targy, Newx, Newy, [H|T1]) :-
	replace(T, Targx, Targy, Newx, Newy, T1).

start(0, [2/3, 1/3, 2/2, 3/3, 3/2, 3/1, 2/1, 1/1, 1/2]). %  1 step
start(1, [2/2, 1/3, 3/2, 2/3, 3/3, 3/1, 2/1, 1/1, 1/2]). %  4 steps
start(2, [1/3, 2/3, 2/2, 3/3, 3/2, 3/1, 1/2, 2/1, 1/1]). %  6 steps
start(3, [1/1, 1/3, 2/2, 2/3, 3/3, 3/2, 2/1, 1/2, 3/1]). % 10 steps
start(4, [2/2, 1/2, 3/3, 3/2, 2/1, 3/1, 1/1, 1/3, 2/3]). % 12 steps
start(5, [1/2, 1/1, 2/2, 1/3, 2/3, 3/3, 3/2, 3/1, 2/1]). % 15 steps
start(6, [1/1, 1/3, 2/1, 2/3, 3/2, 3/1, 2/2, 3/3, 1/2]). % 16 steps
start(7, [2/2, 1/2, 2/1, 3/2, 1/3, 3/1, 2/3, 1/1, 3/3]). % 18 steps
start(8, [3/1, 1/3, 3/2, 2/1, 3/3, 2/3, 1/1, 2/2, 1/2]). % 20 steps
start(9, [2/2, 3/2, 3/3, 1/3, 1/2, 2/3, 1/1, 2/1, 3/1]). % 24 steps
start(10,[2/2, 1/3, 1/2, 1/1, 3/1, 2/3, 2/1, 3/3, 3/2]). % 26 steps
start(11,[1/3, 3/1, 3/2, 1/1, 2/1, 1/2, 2/3, 3/3, 2/2]). % 28 steps

%------------------------------------------------------------
% Generating random boards

% Increasing value of Shuffles = greater probability of
%    longer path.
random_board(Shuffs, Board):-
	solution(S),
	random_moves(Shuffs, S, Board).

random_moves(0, S, S) :- !.
random_moves(N, S0, S) :-
	findall(S1, succ(S0, _, S1), Lst),
	random_choice(Lst, S2),
	N1 is N - 1,
	random_moves(N1, S2, S).

random_choice(List, Item) :-
	length(List, N),
	I is random(N),
	nth0(I, List, Item).

%------------------------------------------------------------
% Predicates for displaying the board.

% show board textually
show([S0, S1, S2, S3, S4, S5, S6, S7, S8]) :-
	member(Y, [3,2,1]),
	nl,
	member(X, [1,2,3]),
	member(Tile-X/Y,
	      [' '-S0, 1-S1, 2-S2, 3-S3, 4-S4, 5-S5, 6-S6, 7-S7, 8-S8]),
	write(Tile),
	fail
	;
	nl,
	true.

% show board graphically
showg([S0, S1, S2, S3, S4, S5, S6, S7, S8]) :-
	new(W, picture('Eight Puzzle')),
	send(W, size, size(330,330)),
	send(W, open),
	member(Tile-X/Y,
	      [' '-S0, 1-S1, 2-S2, 3-S3, 4-S4,
	       5-S5, 6-S6, 7-S7, 8-S8]),
	show_tile(W, Tile, X, Y),
	fail.
showg(_).

show_tile(Win, Tile, X, Y):-
	Xpos is (X-1)*101+20,
	Ypos is (3-Y)*101+20,
	send(Win, display, new(Box, box(100,100)), point(Xpos, Ypos)),
	send(Box, pen, 2),
	XText is Xpos + 40, YText is Ypos + 30,
	send(Win, display, new(Text, text(Tile)), point(XText,YText)),
	send(Text, font, font(helvetica, bold, 24)).

%--------------------------------------------------------------------
% heuristic function


