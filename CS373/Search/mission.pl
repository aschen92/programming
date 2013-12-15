% mission.pl -- missionaries and cannibals as general search problem.
% by: John Zelle

/* Problem Representation:

mcb(M,C,B) 
   M is the number of missionaries on the original side of the river
   C is the number of cannibals on the original sode of the river
   B is either -1 or 1 to represent which side the boat is on. When B is
     -1, the boat is on the original side, and numbers are subtracted,
     when B is 1, the boat is on the other side, so numbers will be added.
*/

start(mcb(3,3,-1)). % 3 missionaries, 3 cannibals, boat is set to remove some.

solution(mcb(0,0,1)). % All are now on the other side of the river.


% Five possible operators.
succ( mcb(M,C,B), mm, mcb(M1, C, B1) ) :- % send two missionaries
	M1 is M + 2 * B,
	legal(M1,C),
	B1 is -B.

succ( mcb(M,C,B), cc, mcb(M,C1,B1) ) :- % send two cannibals
	C1 is C + 2 * B,
	legal(M,C1),
	B1 is -B.

succ( mcb(M,C,B), mc, mcb(M1,C1,B1) ) :-   % send one of each
	M1 is M + B,
	C1 is C + B,
	legal(M1,C1),
	B1 is - B.

succ( mcb(M,C,B), m, mcb(M1, C, B1) ) :-   % send one missionary
	M1 is M + B,
	legal(M1,C),
	B1 is -B.

succ( mcb(M,C,B), c, mcb(M,C1,B1) ) :-     % send on cannibal
	C1 is C + B,
	legal(M,C1),
	B1 is -B.

% legal(M,C) -- M and C represent an allowable configuration of 
%               missionaries and cannibals.
legal(M,C) :-
	between(0,3,M),  % between 0 and 3 missionaries
	between(0,3,C),  % between 0 and 3 cannibals
	safe(M,C),       % missionaries on this side are safe
	M1 is 3 - M,     % number of missionaries on other side
	C1 is 3 - C,     % number of cannibals on other side
	safe(M1,C1).     % missionaries on the other side are safe

safe(0,_) :- !.  % Safe if there are no missionaries.
safe(M,C) :-     % Safe if there are at least as many missionaries.
	M >= C.


