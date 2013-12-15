% Aaron Schendel

% foo(1,2) --> foo(X,Y)  == X=1,Y=2
% foo(X,4) --> foo(5,Y)  == X=4, Y=5
% foo(X,4) --> foo(5,X)  == Cannot unify.
% foo(bar(1,2),X) --> foo(bar(Y,Z),baz(3))  == Cannot unify.


% prefix([1,2], [1,2,3,4,5]).
prefix([],_).
prefix([H|T1],[H|T2]):-
	prefix(T1,T2).

prefix1(L1,L2):- append(L1, _, L2).

% suffix([4,5,6],[1,2,3,4,5,6]).
suffix(S,S).
suffix(L,[_|T]):-
	suffix(L,T).

suffix1(L1,L2):- append(_, L1, L2).

% last([1,2,3,4],4).
last(L, Item):-
	append(_,[Item],L).


% sublist([2,3,4],[1,2,3,4,5]).
sublist(L1,L2):-
	append(FirstPart,_,L2),
	append(_,L1,FirstPart).
	

% reverse([1,2,3], Reversed).    Recursion.
reverse([],[]).
reverse([H|T], Reversed):-
	reverse(T,TR),
	append(TR,[H],Reversed).

% reverse1([1,2,3], Reversed).   Iterative.
revLoop1([], SoFar, SoFar).
revLoop1([H|T], SoFar, Rev):-
	revLoop1(T,[H|SoFar], Rev).

reverse2(List, Reverse):-
	revLoop1(List,[],Reverse).

% lshift([1,2,3] ,RotatedLeft).
lshift([H|T], Lshifted):-
	append(T,[H],Lshifted).

% rshift([1,2,3], RotatedRight).
rshift(List , Rshifted):-
	last(List,Last),
	append(Something, [Last], List),
	append([Last], Something, Rshifted).

%translate([3,1,4],Symbols).
symbol(0,zero).
symbol(1,one).
symbol(2,two).
symbol(3,three).
symbol(4,four).
symbol(5,five).
symbol(6,six).
symbol(7,seven).
symbol(8,eight).
symbol(9,nine).
translate([],[]).
translate([H|T], [Symb|T1]):-
	symbol(H,Symb),
	translate(T,T1).


% double([1,2,3],Doubled).
double([],[]).
double([H|T],DoubledList) :-
	double(T,Sofar),
	append([H,H],Sofar,DoubledList).


% sum([1,2,3], 6).
sum([],0).
sum([H|T],Total):-
	sum(T, Sofar),
	Total is H + Sofar.


% evenLength(List).
evenLength([]).
evenLength([_,_|T]):-
	evenLength(T).

% evenLength(List).
oddLength(List):-
	\+ evenLength(List).


% min(Min, List).
min(N,[N]).
min(Min, [H|T]):-
	min(MinT, T),
	smaller(H, MinT, Min).

smaller(X1, X2, X1):-
	X1< X2, !.
smaller(_, X2, X2).


% max(Max,[1,2,3,4,5]).
max(M,[H|T]):-
	max_loop(T, H, M).

max_loop([], Ans, Ans).
max_loop([H|T], AnsSoFar, M):-
	bigger(H,AnsSoFar,AnsSoFar1),
	max_loop(T,AnsSoFar1,M).


bigger(X1, X2, X1):-
	X1 > X2, !.
bigger(_, X2, X2).
	


% deleteAll(List,X,ListwXRemoved).
deleteAll([],_,[]).
deleteAll([Target|T], Target, T1):-
	!,
	deleteAll(T,Target,T1).
deleteAll([H|T],Target, [H|T1]):-
	H \= Target,
	deleteAll(T,Target,T1).


% select(X,List,OneLessX).
select(Target,[Target|T],T).
select(Target, [A|T], [A|T1]):-
	Target \== A,
	select(Target, T, T1).
	



% split(list, first, second).
split([],[],[]).
split([H],[H],[]).
split([H1,H2|Rest], [H1|T1], [H2|T2]):-
	split(Rest,T1,T2).


% insert(X, List, Result). 
insert(X, List, [X|List]).
insert(X, [H|T], [H|T1]):-
	insert(X,T,T1).


% partition(List,X,Littles, Bigs).
partition([], _, [], []).
partition([H|T], X, SmallLst, [H|BigLst]):-
	H > X,
	partition(T,X,SmallLst,BigLst).

partition([H|T], X, [H|SmallLst], BigLst):-
	H =< X,
	partition(T,X,SmallLst,BigLst).
	


% quicksort(List, SortedList). - partition
quicksort([],[]).
quicksort([H|T], Sorted):-
	partition(T, H, SmallLst, BigLst),
	quicksort(SmallLst, SmallSorted),
	quicksort(BigLst, BigSorted),
	append(SmallSorted, [H|BigSorted], Sorted).