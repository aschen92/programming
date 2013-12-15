% random_agt.pl
:- module(agent,[act/2, set_id/1]).  % module to insure no collision with other code.
:- use_module(alists).

:- dynamic my_id/1.

set_id(I) :-
	retractall(my_id(_)),
	asserta(my_id(I)).

act(_, _) :- random(X), sleep(X), fail.  % first delay for up to 1 second

act(P, charge) :-		% reflex charge
	%nl,writeln('Trying Charge'),
	my_id(I),
	lookup(P, [pos(I)-Pos, max_charge-MC, chargers-Cs,
		   charge(I)-Charge, home(I)-Home]),
	Charge < MC-1,
	(memberchk(Pos, Cs) ; Pos = Home).

act(P, suck) :-  % reflex suck
	%writeln('Trying Suck'),
	my_id(I),
	lookup(P, [pos(I)-Pos, dirt-DL, charge(I)-Charge, capacity(I)-Cap]),
	Cap > 0,
	Charge > 0,
	memberchk(Pos, DL).

act(P, dump) :- %reflex dump
	%writeln('Trying Dump'),
	my_id(I),
	lookup(P, [pos(I)-Pos, home(I)-Pos,
		   capacity(I)-Cap, max_capacity-MC]),
	Cap < MC.

act(_, Move) :- % random movement
	%writeln('Doing Move'),
	X is random(3),
	nth0(X, [left,right,forward], Move).

