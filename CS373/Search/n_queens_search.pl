% R/C

start([]).

problem_size(10).


succ(B0,R1,[R1/C1|B0]):-
	B0 = [R0/C0|_],!,
	problem_size(N),
	between(1,N,R1),
	C1 is C0 + 1.

succ([], R, [R/1]):-
	problem_size(N),
	between(1,N,R).


solution(B):-
	length(B,N),
	problem_size(N),
	no_attacks(B).

no_attacks([]).
no_attacks([R/C|T]):-
	safe_from_all(T,R,C),
	no_attacks(T).

safe_from_all([],_,_).
safe_from_all([R1/C1|T],R,C):-
	R1 \= R,
	D1 is abs(R1-R),
	D2 is abs(C1-C),
	D1 \= D2,
	safe_from_all(T,R,C).