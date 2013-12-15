% n-queens using clpfd
:- use_module(library(clpfd)).

% solution(N, Qs) Qs is a list of row positions for successive
%     columns in an N-Queens problem.
solution(N, Qs) :-
	length(Qs,N),    % creates a list of length N
	% set up domain(s) for variables in Qs
	% establish contraint that each queen must be in a different row
	% establish pairwise contraints no two queens on same diagonal
	% reqeust a labeling of Qs, using fast-fail [ff].


% diag_safe(Qs) Establish contraints so no two queens in Qs are on same diagonal
diag_safe([]).


% diag_safe(Qs, Q, Distance) Q is a queen position that is Distance
%    columns away from remainder queens in list Qs. This predicate
%    establishes a "differing diagonal" contraint between Q and every
%    queen in Qs.

diag_safe([], _Q, _D).
