:- module(agent, [act/2, set_id/1]).
% manual_agt.pl


% a text interface to drive an agent around.

:- dynamic my_id/1.

set_id(I) :-
	retractall(my_id(_)),
	asserta(my_id(I)).


act(_, Action) :- read(Action).