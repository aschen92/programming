% family.pl

parent(pam,bob).
parent(tom,bob).
parent(bob,ann).
parent(bob,pat).
parent(pat,jim).

female(pam).
female(liz).
female(ann).
female(pat).

male(tom).
male(bob).
male(jim).


mother(X,Y) :-
	parent(X,Y),
	female(X).

sibling(X,Y) :-
	parent(P,Y),
	parent(P,X),
	X \= Y.

sister(X,Y) :-
	sibling(X,Y),
	female(X). 

grandparent(X,Y) :-
	parent(P,Y),
	parent(X,P). 

greatgrandparent(X,Y) :-
	parent(P,Y),
	grandparent(X,P). 

entwined(X,Y) :-
	parent(X,Child),
	parent(Y,Child),
	X \= Y. 

uncle(Uncle, N) :-
	parent(Par,N),
	sibling(Uncle,Par),
	male(Uncle). 
uncle(Uncle, N) :-
	parent(Par,N),
	sibling(Par, Aunt),
	entwined(Aunt, Uncle),
	male(Uncle). 

cousin(X,Y) :-
	grandparent(GP, X),
	grandparent(GP, Y),
	X \= Y,
	\+sibling(X,Y). 

ancestor(X,Y) :-          % Base case. 
	parent(X,Y).			     
ancestor(X,Y) :-          % Recursive rule. 
	parent(P,Y),
	ancestor(X,P).		


% definitions of predecessor relation


% Note: the first 4 versions have the same logical "meaning"

% Reading: X is a predecessor of Z if X is a parent of Z
%          X is a predecessor of Z if X is a parent of some child Y
%                  and Y is a predecessor of Z.

pred1(X,Z) :-
	parent(X,Z).
pred1(X,Z) :-
	parent(X,Y),
	pred1(Y,Z).

% same as pred1 with clauses swapped
pred2(X,Z) :-
	parent(X,Y),
	pred2(Y,Z).
pred2(X,Z) :-
	parent(X,Z).

% same as pred1, but with goals swapped in second clause
pred3(X,Z) :-
	parent(X,Z).
pred3(X,Z) :-
	pred3(Y,Z),
	parent(X,Y).

% same as pred3, but with clauses swapped
pred4(X,Z) :-
	pred4(Y,Z),
	parent(X,Y).
pred4(X,Z) :-
	parent(X,Z).

% Here is a different definition
pred5(X, Z):-
	parent(X, Z).
pred5(X, Z) :-
	parent(Y, Z),
	pred5(X, Y).

% and a variation
pred6(X, Z) :-
	parent(Y, Z),
	pred6(X, Y).
pred6(X, Z):-
	parent(X, Z).
