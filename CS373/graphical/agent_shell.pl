% psagent-shell.pl

%    Simplified problem-solving agent for vacuum world. This agent
%    does not have a separate update-state or goal-formulation
%    step. It relies on the current percept being sufficient to formulate
%    an appropriate search problem.

:-module(agent,[act/2, set_id/1]).  % module to insure no collision with other code.

% include whatever you need
:- use_module(sets).    
:- use_module(queues).
:- use_module(alists).

%---------------------------------------------------------------------------
% write all of the terms in Terms (a list) to the file debug.txt
%    use this to do debugging writes while running the graphical env
debug(Terms) :-
	open('debug.txt', append, Fd),
	tell(Fd),
	write_terms(Terms),
	told.

write_terms([]) :- nl.
write_terms([H|T]) :- write(H), write(' '), write_terms(T).

%---------------------------------------------------------------------------
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


%Safety predicate to prevent crashes
act(Percept, Action) :- catch(act1(Percept,Action), E, handleError(E,Action)).

handleError(Exception, Action) :-
	debug(['Exception thrown:', Exception]),
	% need to compute some action here, this is a fallback
	Action=error.

%---------------------------------------------------------------------------
% Action problem solving agent code
	
act1(Percept, Action):-		% if plan is not empty, just use it
	retract(plan([Action|Rest])),
	!,
	assert(plan(Rest)).
act1(Percept, Action) :-		% otherwise, make a new plan, Stan.
	retract(plan([])),
	formulate_plan(Percept, [Action|Actions]),
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














	
	

