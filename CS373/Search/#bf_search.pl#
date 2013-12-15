% bf_search.pl: breadth-first search algorithm.
% Programmer: John Zelle

:- use_module(queues).


/* Assumes problem defined by:
*
*      succ(State0, Op, State1) -- State1 is a successor of State0, via
*                                  application of Op
*      solution(State) -- State is a solution (goal state).
*/

/* bf_search(+Start, -Final)
     Start is the initial state
     Final is the solution state found
*/

bf_search(Start, Final) :-
	queue_empty(EmptyQ),
	search_loop(Start, EmptyQ, Final).

/* search_loop(+Current, +FrontierQ, -Final)
      Current is the state currently being considered
      FrontierQ is a queue of states that have been generated but not
          yet considered
      Final is the solution state reached
*/ 

search_loop(S, _, S) :- solution(S).
search_loop(S0, Frontier0, S) :-
	findall(S1, succ(S0,_,S1), Children),
	enqueue_list(Children, Frontier0, Frontier1),
	dequeue(S2, Frontier1, Frontier),
	search_loop(S2, Frontier, S).

%---------------------------------------------------------------------------
% This version returns solution sequence (plan)

bf_search(Start, Final, Ops) :-
	queue_empty(EmptyQ),
	search_loop(Start, [], EmptyQ, Final, Ops).

/* search_loop(+CurrentState, +RevOpsSoFar, +FrontierQ, -FinalState, -FinalOps)
     CurrentState is the state currently being examined
     RevOpsSoFar is the sequence of operations (in reverse order) leading
       to CurrentState
     FrontierQ is a queue of n(State,RevOps) terms holding as yet unexpanded
       states and the operator sequence (in reverse) leading to State
     FinalState is the last state reached in the breadth-first examination so far
     FinalOps is the sequence of operators leading to the final state.
*/

search_loop(S, RevOps, _Frontier, S, Ops) :-
	solution(S),
	reverse(RevOps, Ops).

search_loop(S0, Ops0, Frontier0, S, Ops) :-
	findall(n(S1,[Op|Ops0]), succ(S0,Op,S1), Children),
	enqueue_list(Children, Frontier0, Frontier1),
	dequeue(n(S2, Ops2), Frontier1, Frontier),
	search_loop(S2, Ops2, Frontier, S, Ops).
