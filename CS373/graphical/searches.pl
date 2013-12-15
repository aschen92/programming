% searches.pl
%    Generalized versions of some search algorithms
% by: John Zelle
%
%  These all take a search problem specification as the
%      first argument. So the same algorithm can be used within
%      a program to solve different search problems.

% A search problem is specified as a term: sp(StartState, SuccPred, GoalPred)
%    Where SuccPred and GoalPred are atoms that name the appropriate predicates. 
%    For example, to use our previous search formulation you could do:
%        start(S),
%        Problem = sp(S, succ, solution)
%        bf_search(Problem, FinalState, Ops).

%------------------------------------------------------------
% breadth-first search

bf_search(sp(Start, Succ, GoalTest), Final, Path) :-
	queue_empty(Open),
	gen_candidate(Succ, Start, [], Open, Final, RevPath),
	call(GoalTest,Final),
	reverse(RevPath, Path).

gen_candidate(_, S, Path, _, S, Path).
gen_candidate(SuccPred, S0, Path0, Open0, S, Path) :-
	findall(n(S1,[Op|Path0]), call(SuccPred,S0,Op,S1), Children),
	enqueue_list(Children, Open0, Open1),
	dequeue(n(S2, Path2), Open1, Open),
	gen_candidate(SuccPred, S2, Path2, Open, S, Path).

%-------------------------------------------------------------
% breadth-first graph search

bfg_search(sp(Start,Succ,GoalTest), Final, Path) :-
	queue_empty(Open),
	list_to_set([Start], Closed),
	gen_candidate(Succ, Start, [], Open, Closed, Final, RevPath),
	call(GoalTest, Final),
	reverse(RevPath, Path).

gen_candidate(_, S, Path, _, _, S, Path).
gen_candidate(SuccPred, S0, Path0, Open0, Closed0, S, Path) :-
	% generate child nodes for previously unseen states
	findall(n(S1,[Op|Path0]),
		(call(SuccPred,S0,Op,S1), \+set_memberchk(S1,Closed0)),
		Children),
	% mark all of these states as generated
	findall(State, member(n(State,_), Children), NewStates),
	set_add_list(Closed0, NewStates, Closed),
	% put the nodes on the queue
	enqueue_list(Children, Open0, Open1),
	dequeue(n(S2, Path2), Open1, Open),
	gen_candidate(SuccPred, S2, Path2, Open, Closed, S, Path).


%------------------------------------------------------------
% Iterative deepening searches

max_depth(50).			% Max depth to search to.

% iterative deepening search to max_depth
id_search(sp(Start, SuccPred, GoalTest), Final, Ops) :-
	max_depth(Max),
	between(0, Max, Depth),
	%writeln(Depth),
	path(SuccPred, Start, Depth, Final, Ops),
	call(GoalTest,Final).

path(_, S, _, S, []).
path(SuccPred, S0, StepsLeft, Final, [Op|Ops]) :-
	StepsLeft > 0,
	StepsLeft1 is StepsLeft - 1,
	call(SuccPred, S0, Op, S1),
	path(SuccPred, S1, StepsLeft1, Final, Ops).

% Graph search (cycle checking).
idg_search(sp(Start, SuccPred, GoalTest), Final, Ops) :-
	max_depth(Max),
	between(0, Max, Depth),
	%writeln(Depth),
	list_to_set([Start], Closed),
	path(SuccPred, Start, Depth, Closed, Final, Ops),
	call(GoalTest, Final).

path(_, S, _, _, S, []).
path(SuccPred, S0, StepsLeft, Closed0, Final, [Op|Ops]) :-
	StepsLeft > 0,
	StepsLeft1 is StepsLeft - 1,
	call(SuccPred, S0, Op, S1),
	set_add_new(Closed0, S1, Closed),
	path(SuccPred, S1, StepsLeft1, Closed, Final, Ops).

%------------------------------------------------------------
% Iterative deepening with heuristic pruning (ida)
%   Search problem: sp(StartState, SuccPred, GoalPred, HeuristicPred)


ida_search(sp(Start, SuccPred, GoalTest, HPred), Final, Ops) :-
	max_depth(MaxD),
	call(HPred, Start, StartH),
	gen_bound(StartH, MaxD, Depth),
	writeln(Depth),
	path(SuccPred, HPred, Start, StartH, Depth, Final, Ops),
	call(GoalTest, Final).

path(_, _, S, _, _, S, []).
path(SuccPred, HPred, S0, H0, StepsLeft, S, [Op|Ops]) :-
	StepsLeft >= H0,
	StepsLeft > 0,
	!,
	StepsLeft1 is StepsLeft - 1,
	call(SuccPred, S0, Op, S1),
	call(HPred, S1, H1),
	path(SuccPred, HPred, S1, H1, StepsLeft1, S, Ops).
path(_,_, _, H, StepsLeft, _, _) :-   % pruned. update bound, then fail.
	% H > StepsLeft or StepsLeft and H are 0
	Needed is max(H-StepsLeft,1),
	update_next_bound(Needed),
	fail.

%------------------------------------------------------------
% version for graph search (cycle checking)

idag_search(sp(Start, SuccPred,GoalTest, HPred), Final, Ops) :-
	max_depth(MaxD),
	call(HPred, Start, StartH),
	gen_bound(StartH, MaxD, Depth),
	%writeln(Depth),
	list_to_set(Start, Closed),
	path(SuccPred, HPred, Start, StartH, Depth, Closed, Final, Ops),
	call(GoalTest, Final).

path(_, _, S, _, _, _, S, []).
path(SuccPred, HPred, S0, H0, StepsLeft, Closed0, S, [Op|Ops]) :-
	StepsLeft >= H0,
	StepsLeft > 0,
	!,
	StepsLeft1 is StepsLeft - 1,
	call(SuccPred, S0, Op, S1),
	set_add_new(Closed0, S1, Closed),
	call(HPred, S1, H1),
	path(SuccPred, HPred, S1, H1, StepsLeft1, Closed, S, Ops).
path(_, _, _, H, StepsLeft, _, _, _) :-   % pruned. update bound, then fail.
	% H > StepsLeft or both are 0
	Needed is max(H-StepsLeft,1),   
	update_next_bound(Needed),
	fail.


%------------------------------------------------------------
% Extra-logical predicates to generate/update next bound.

/* gen_bound(+Start, +Max, -Bound) -- generate successive depth bounds
       between Start and Max via backtracking. The increase on a redo
       is determined by the value of the dynamic predicate bound_increment/1.
*/

:- dynamic bound_increment/1.

gen_bound(Start, Max, Start):-
	retractall(bound_increment(_)),
	asserta(bound_increment(Max)).  % set increment to "infinity"
gen_bound(Bound, Max, Bound1) :-
	bound_increment(Inc),
	NewBound is Bound + Inc,
	NewBound < Max,
	gen_bound(NewBound, Max, Bound1).

% Save the smallest non-zero increment necessary to usefully expand search
update_next_bound(ThisInc) :-
	bound_increment(Inc),
	ThisInc < Inc,
	!,
	retractall(bound_increment(_)),
	asserta(bound_increment(ThisInc)).
update_next_bound(_).
