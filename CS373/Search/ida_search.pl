% ida_search.pl -- Iterative deepening heuristic search.
% by: John Zelle

% Attempt to instrument id_search for idastar.

% input problem: succ/3, solution/1, h/2.
%

max_depth(40).			% Max depth to search to.

ida_search(Start, Final, Ops) :-
	max_depth(MaxD),
	h(Start, StartH),
	gen_depth_bound(StartH, MaxD, Depth),
	write(Depth),nl,
	dbh_search(Start, StartH, Depth, Final, Ops).


dbh_search(S, _, _, S, []) :- solution(S).
dbh_search(S0, H0, StepsLeft, S, [Op|Ops]) :-
	StepsLeft >= H0,
	!,
	StepsLeft1 is StepsLeft - 1,
	succ(S0, Op, S1),
	h(S1, H1),
	dbh_search(S1, H1, StepsLeft1, S, Ops).
dbh_search(_, H, StepsLeft, _, _) :-   % pruned. update bound, then fail.
	% H > StepsLeft
	Needed is H-StepsLeft,
	update_bound_increment(Needed),
	fail.

% Extra-logical predicates to handle generating/updating next bound.

/* gen_depth_bound(+Start, +Max, -Bound) -- generate successive depth bounds
       between Start and Max via backtracking. The increase on a redo
       is determined by the value of the dynamic predicate bound_increment/1.
*/

gen_depth_bound(Start, Max, Start):-
	retractall(bound_increment(_)),
	asserta(bound_increment(Max)).  % set increment to "infinity"
gen_depth_bound(Bound, Max, Bound1) :-
	bound_increment(Inc),
	NewBound is Bound + Inc,
	NewBound < Max,
	gen_depth_bound(NewBound, Max, Bound1).

% Save the smallest increment necessary to usefully expand search
update_bound_increment(ThisInc) :-
	bound_increment(Inc),
	ThisInc < Inc,
	!,
	retractall(bound_increment(_)),
	asserta(bound_increment(ThisInc)).
update_bound_increment(_).







