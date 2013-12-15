% snd.pl

%    Simplified problem-solving agent for vacuum world. This agent
%    does not have a separate update-state or goal-formulation
%    step. It relies on the current percept being sufficient to formulate
%    an appropriate search problem.

:-module(agent,[act/2, set_id/1]).  % module to insure no collision with other code.

% include whatever you need
:- use_module(sets).    
:- use_module(queues).
:- use_module(alists).

:- [searches].


debug(Terms) :-
	open('debug.txt', append, Fd),
	tell(Fd),
	write_terms(Terms),
	told.

write_terms([]) :- nl.
write_terms([H|T]) :- write(H), write(' '), write_terms(T).
	


/* my_id indicates which agent this is in the percept. The simulator will call
   set_id/1 prior to any calls to act/2. The id is a (1-based) index
   into the list of agents.
*/

:- dynamic my_id/1.

my_id(1).

set_id(I) :-
	retractall(my_id(_)),
	asserta(my_id(I)).


/* This will be our plan */
:- dynamic plan/1.
plan([]).			% initially, no plan

/************************************************************
  Simplified problem solving agent: act(+Percept, -Action)
  Note: The percept is just an environment state. 
*/

act(_Percept, Action):-		% if plan is not empty, just use it
	retract(plan([Action|Rest])),
	!,
	debug(['Have a plan', Action]),
	assert(plan(Rest)).
act(Percept, Action) :-		% otherwise, make a new plan, Stan.
	retractall(plan(_)),
	debug(['Planning...', Percept]),
	formulate_plan(Percept, [Action|Actions]),
	debug(['Found plan', [Action|Actions]]),
	assert(plan(Actions)).

/******************************************************************
* Useful background knowledge mostly borrowed from mvw_sim
*/

can_move(Charge) :- Charge > 0, !.
can_move(_) :- rules(Rules), lookup(Rules, aux_battery-true).

next_position(east, xy(X,Y), xy(X1,Y)) :- X1 is X + 1.
next_position(west, xy(X,Y), xy(X1,Y)) :- X1 is X - 1.
next_position(north, xy(X,Y), xy(X,Y1)) :- Y1 is Y - 1.
next_position(south, xy(X,Y), xy(X,Y1)) :- Y1 is Y + 1.

legal_position(S, xy(X,Y)) :- between(1,S,X), between(1,S,Y).

clockwise_next(north, east).
clockwise_next(east, south).
clockwise_next(south, west).
clockwise_next(west, north).

del_one(X, [X|Y], Y) :- !.
del_one(X, [H|T], [H|T1]) :-
 	del_one(X,T,T1).


%---------------------------------------------------------------------------
/* This simple agent can be completed by defining:

       formulate_plan(+Percept, -Plan)

   A "standard" problem solving agent would do this as a two-step
   process of:
   
       formulate_search(+Percept, -SearchProblem) to formulate an
             appropriate search problem, given the current state.
   
       search(+SearchProblem, -Actions) to do the search, you may want
             to borrow code from searches.pl to do this.
*/


% if we're home and there's dirt left, go find some and suck
formulate_plan(Percept, Plan):-
	my_id(I),
	lookup(Percept, [pos(I)-Pos, home(I)-Pos, dirt-Dirt]),
	Dirt = [_|_],
	!,
	get_rf_plan(Percept, Dirt, Plan0),
	append([charge|Plan0], [suck], Plan).

% if not home, go home and dump
formulate_plan(Percept, Plan) :-
	my_id(I),
	lookup(Percept, [pos(I)-Pos, home(I)-Home]),
	Pos \= Home,
	!,
	get_rf_plan(Percept, [Home], Plan0),
	append(Plan0, [dump], Plan).


% use percept to get starting info, find plan to Dests
get_rf_plan(Percept, Dests, Plan):-
	make_rfstate(Percept, Dests, S0),
	debug([initial,rf_search, state, S0]),
	idg_search(sp(S0,rf_succ, rf_solution), _, Plan).

	
%---------------------------------------------------------------------------
% route-finding search stuff

make_rfstate(Percept, Dests, rfstate(Size, Pos, Dir, Obstacles, Dests)):-
	my_id(I),
	OID is 3-I,
	lookup(Percept, [size-Size, pos(I)-Pos, dir(I)-Dir,
			 furniture-Furn, pos(OID)-Opp]),
	Obstacles = [Opp|Furn].

rf_solution(rfstate(_Size,Pos,_D,_Obs,Dests)) :-
	memberchk(Pos, Dests).

rf_succ(rfstate(Size, Pos, Dir, Obstacles, Dests),
	right,
	rfstate(Size, Pos, Dir1, Obstacles, Dests)) :-
	clockwise_next(Dir, Dir1).

rf_succ(rfstate(Size, Pos, Dir, Obstacles, Dests),
	left,
	rfstate(Size, Pos, Dir1, Obstacles, Dests)) :-
	clockwise_next(Dir1, Dir).

rf_succ(rfstate(Size, Pos, Dir, Obstacles, Dests),
	forward,
	rfstate(Size, Pos1, Dir, Obstacles, Dests)) :-
	next_position(Dir, Pos, Pos1),
	legal_position(Size, Pos1),
	\+ memberchk(Pos1, Obstacles).

make_search_problem(Start, Succ, Solution, sp(Start, Succ, Solution)).


%------------------------------------------------------------
% Iterative deepening searches

%max_depth(50).			% Max depth to search to.

% iterative deepening search to max_depth
% id_search(sp(Start, SuccPred, GoalTest), Final, Ops) :-
% 	max_depth(Max),
% 	between(0, Max, Depth),
% 	debug([depth, Depth]),
% 	path(SuccPred, Start, Depth, Final, Ops),
% 	call(GoalTest,Final).

% path(_, S, _, S, []).
% path(SuccPred, S0, StepsLeft, Final, [Op|Ops]) :-
% 	StepsLeft > 0,
% 	StepsLeft1 is StepsLeft - 1,
% 	call(SuccPred, S0, Op, S1),
% 	path(SuccPred, S1, StepsLeft1, Final, Ops).













	
	

