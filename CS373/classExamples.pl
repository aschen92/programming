:- use_module(library(lists)).

% in class learning, 9/18/12

member(H,[H|T]).

member(X,[H|T]):-
	member(X,T).


%-------------------------------

%add_front(Item,List, ListWithItemAtFront).
%add_front(3,[1,4,1,5],[3,1,4,1,5]).

add_front(H,T,[H|T]).


add_back(X, [], [X]).
add_back(X,[H|T1],[H|T2]):-
	add_back(X,T1,T2).


%---------------------------------

above(a,b).
above(b,c).
above(d,e).
above(e,f).
above(g,a).
above(a,c).
above(b,d).
above(d,h).
above(i,c).
above(c,e).

precedes(X,Y):-
	above(X,Y).

precedes(X,Z):-
	above(X,Y),
	precedes(Y,Z).

%ok(List) -- holds when ordering respects known precedes relation

ok([]).
ok([H|T]) :-
	none_precede(T|H),
	ok(T).
% -- holds if no branch in branchlist precedes Branch
% none_precede(BranchList, Branch):-

none_precede([],_).
none_precede([H|T], B):-
	\+ precedes(H,B),
	none_precede(T,B).

solution(L):-
	permutation([a,b,c,d,e,f,g,h,i], L),
	ok(L).

count(N):-
	findall(L, solution(L), Ls),
	length(Ls, N).

%-----------------------------------------------------------