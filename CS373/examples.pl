:- use_module(library(clpfd)).

%------------------------------------------------------------
% Better arithmetic

fact(0,1):-!.
fact(N, F) :-
	N1 #= N-1,
	F #= N * F1,
	F #> 0,
	fact(N1, F1).

%------------------------------------------------------------
% map coloring

color_australia(Vars) :-
	Vars = [WA, SA, NT, Q, NSW, V, _T],
	Vars ins 1..3,
	SA #\= WA,
	SA #\= NT,
	SA #\= Q,
	SA #\= NSW,
	SA #\= V,
	WA #\= NT,
	NT #\= Q,
	Q #\= NSW,
	NSW #\= V.
	%label(Vars).

%------------------------------------------------------------
% Sudoku similar to version in AIMA
sudoku_board(B) :-
	B = [ A1, A2, A3, A4, A5, A6, A7, A8, A9,
	      B1, B2, B3, B4, B5, B6, B7, B8, B9,
	      C1, C2, C3, C4, C5, C6, C7, C8, C9,
	      D1, D2, D3, D4, D5, D6, D7, D8, D9,
	      E1, E2, E3, E4, E5, E6, E7, E8, E9,
	      F1, F2, F3, F4, F5, F6, F7, F8, F9,
	      G1, G2, G3, G4, G5, G6, G7, G8, G9,
	      H1, H2, H3, H4, H5, H6, H7, H8, H9,
	      I1, I2, I3, I4, I5, I6, I7, I8, I9 ],

	B ins 1..9,

	% row constraints
	all_different([A1, A2, A3, A4, A5, A6, A7, A8, A9]),
	all_different([B1, B2, B3, B4, B5, B6, B7, B8, B9]),
	all_different([C1, C2, C3, C4, C5, C6, C7, C8, C9]),
	all_different([D1, D2, D3, D4, D5, D6, D7, D8, D9]),
	all_different([E1, E2, E3, E4, E5, E6, E7, E8, E9]),
	all_different([F1, F2, F3, F4, F5, F6, F7, F8, F9]),
	all_different([G1, G2, G3, G4, G5, G6, G7, G8, G9]),
	all_different([H1, H2, H3, H4, H5, H6, H7, H8, H9]),
	all_different([I1, I2, I3, I4, I5, I6, I7, I8, I9]),

	% column constraints
	all_different([A1, B1, C1, D1, E1, F1, G1, H1, I1]),
	all_different([A2, B2, C2, D2, E2, F2, G2, H2, I2]),
	all_different([A3, B3, C3, D3, E3, F3, G3, H3, I3]),
	all_different([A4, B4, C4, D4, E4, F4, G4, H4, I4]),
	all_different([A5, B5, C5, D5, E5, F5, G5, H5, I5]),
	all_different([A6, B6, C6, D6, E6, F6, G6, H6, I6]),
	all_different([A7, B7, C7, D7, E7, F7, G7, H7, I7]),
	all_different([A8, B8, C8, D8, E8, F8, G8, H8, I8]),
	all_different([A9, B9, C9, D9, E9, F9, G9, H9, I9]),

	%cell constraints
	all_different([A1, A2, A3, B1, B2, B3, C1, C2, C3]),
	all_different([D1, D2, D3, E1, E2, E3, F1, F2, F3]),
	all_different([G1, G2, G3, H1, H2, H3, I1, I2, I3]),
	
	all_different([A4, A5, A6, B4, B5, B6, C4, C5, C6]),
	all_different([D4, D5, D6, E4, E5, E6, F4, F5, F6]),
	all_different([G4, G5, G6, H4, H5, H6, I4, I5, I6]),
	
	all_different([A7, A8, A9, B7, B8, B9, C7, C8, C9]),
	all_different([D7, D8, D9, E7, E8, E9, F7, F8, F9]),
	all_different([G7, G8, G9, H7, H8, H9, I7, I8, I9]).

show_board([]) :- !.
show_board(B) :-
	length(Row, 9),
	append(Row, Rows, B),
	maplist(show_cell, Row),
	nl,
	show_board(Rows).

show_cell(X) :-
	(var(X) ->
	 write('.')
	;
	 write(X)
	),
	write(' ').

%------------------------------------------------------------
% Solve from file
solve(File) :-
	see(File),
	read(B),
	seen,
	show_board(B), nl,
	writeln('Solution:'),nl,
	sudoku_board(B),
	labeling([ff],B),
	show_board(B).

%------------------------------------------------------------
% version from SWI-Prolog manual (clpfd).

sudoku(Rows) :-
        length(Rows, 9), maplist(length_(9), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
	maplist(all_distinct, Columns),
        Rows = [A,B,C,D,E,F,G,H,I],
        blocks(A, B, C),
	blocks(D, E, F),
	blocks(G, H, I).

length_(L, Ls) :- length(Ls, L).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_distinct([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).
