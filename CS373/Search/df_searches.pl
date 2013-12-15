x% df_searches.pl -- Depth-first, depth-bounded, and iterative
%     deepening search.

% A search problem is specified via succ/3 and solution/1 predicates

% by: John Zelle

/*************************************************************
  df_search(+StartState, -FinalState, -Ops)
      Depth-first search for a path from StartState to FinalState
      Ops is the sequence of operations applied (a plan).

  db_search(+StartState, DepthBound, -FinalState, -Ops)
      Depth-bounded search for a path from StartState to FinalState
      Ops is the sequence of operations applied (a plan).

  id_search(+StartState, +MaxDepth, -FinalState, -Ops)
      iterative-deepening search for a path from StartState to FinalState
      ops os the sequence of operations applied (a plan).
  
 The search problem is defined by:

      succ(State0, Op, State1) -- State1 is a successor of State0, via
                                  application of Op
      solution(State) -- State is a solution (goal state).

 ************************************************************/

% df_search(S, F, Ops)
df_search(S, S, []) :- solution(S).
df_search(S, F, [Op|Ops]) :-
	succ(S, Op, S1),
	df_search(S1, F, Ops).


%   db_search(S, D, F, Ops)
db_search(S, _, S, []) :- solution(S).
db_search(S, D, F, [Op|Ops]) :-
	D > 0,
	succ(S, Op, S1),                  % add explored set
	D1 is D - 1,
	db_search(S1, D1, F, Ops).

% id_search(Start, MaxDepth, Final, Ops)
id_search(Start, Max, Final, Ops) :-
	between(0, Max, Depth),
	write(Depth),nl,
	db_search(Start, Depth, Final, Ops).

