% monkey_search.pl -- monkey and banana as generic search problem

start(state(atdoor, onfloor, atwindow, hasnot)).

solution(state(_,_,_,has)).

succ(   state(middle,onbox,middle,hasnot),
	grasp,
	state(middle,onbox,middle,has) ).

succ(   state(P, onfloor, P, H),
	climb,
	state(P, onbox, P, H) ).

succ(   state(P1,onfloor,P1,H),
	push(P1,P2),
	state(P2,onfloor,P2,H) ) :- loc(P2), P2 \= P1.

succ(   state(P1,onfloor,B,H),
	walk(P1,P2),
	state(P2,onfloor,B,H) ):- loc(P2), P2 \= P1.


loc(atdoor). loc(middle). loc(atwindow).



