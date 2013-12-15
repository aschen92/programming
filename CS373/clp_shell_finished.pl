% Aaron Schendel
% n-queens using clpfd
:- use_module(library(clpfd)).

% solution(N, Qs) Qs is a list of row positions for successive
%     columns in an N-Queens problem.
solution(N, Qs) :-
	length(Qs,N),      % creates a list of length N
	Qs ins 1..N,     % set up domain(s) for variables in Qs.                                1-n is the domain.
	all_different(Qs),  % establish contraint that each queen must be in a different row       all the variables have distinct variables.
	diag_safe(Qs),      % establish pairwise contraints no two queens on same diagonal         check how many columns apart they are. use diag_safe()
	labeling([ff],Qs).  % reqeust a labeling of Qs, using fast-fail [ff].


% diag_safe(Qs) Establish contraints so no two queens in Qs are on same diagonal ---  3 lines of code.
diag_safe([]).   %true for an empty list
diag_safe([H|T]):-
	diag_safe(T, H, 1),
	diag_safe(T).


% diag_safe(Qs, Q, Distance) Q is a queen position that is Distance
%    columns away from remainder queens in list Qs. This predicate
%    establishes a "differing diagonal" constraint between Q and every
%    queen in Qs.
diag_safe([H|T], Q, D):-
	D #\= abs(Q-H),
	D1 is D+1,
	diag_safe(T, Q, D1).

	
diag_safe([], _Q, _D).
