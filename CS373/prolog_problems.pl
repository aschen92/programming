% prefix([1,2], [1,2,3,4,5]).
prefix([],[H|T]).
prefix([H|T1],[H|T2]):-
	prefix(T1,T2).