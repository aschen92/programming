% vw_sim.pl: Vacuum Cleaner World Simulator
% Programmer: John Zelle

% This file contains predicates for generation of random vacuum cleaner
%    world environments, as well as seed code for the search operators.

%---------------------------------------------------------------------------
/* State of the environment consists of:

    Room size,
    List of furniture cells
    Current position
    Current direction (north, east, south, west),
    List of dirty cells,
    Power (on, off)


    A cell is represented by a term xy(X,Y).

   vw(Size, Furniture, VacPos, VacDir, Dirt, Power)
*/

%---------------------------------------------------------------------------
% Parameters for generation of random environments

room_size(10).       
furniture_prob(20).    %   20% Chance of furniture in any cell
dirt_prob(25).         %   25% Probability of dirt in any cell

%---------------------------------------------------------------------------
% Access predicates for components of the environment.

env_size(       vw(X,_,_,_,_,_), X).
env_furniture(  vw(_,X,_,_,_,_), X).
env_vacpos(     vw(_,_,X,_,_,_), X).
env_vacdir(     vw(_,_,_,X,_,_), X).
env_dirt(       vw(_,_,_,_,X,_), X).
env_power(      vw(_,_,_,_,_,X), X).

env_set_vacpos( vw(A,B,_,D,E,F), X, vw(A,B,X,D,E,F) ).
env_set_vacdir( vw(A,B,C,_,E,F), X, vw(A,B,C,X,E,F) ).
env_set_dirt(   vw(A,B,C,D,_,F), X, vw(A,B,C,D,X,F) ).
env_set_power(  vw(A,B,C,D,E,_), X, vw(A,B,C,D,E,X) ).


%---------------------------------------------------------------------------
% Predicate to generate a random environment.  The first version generates
%   an environment using the parameters at the top of the file.  The 
%   second version accepts arguments for these parameters.

random_env(E) :-
	room_size(S),
	furniture_prob(F),
	dirt_prob(D),
	random_env(S,F,D,E).

random_env(S, E) :-
	furniture_prob(F),
	dirt_prob(D),
	random_env(S,F,D,E).


random_env(S, FProb, DProb, Env) :-
	env_size(Env, S),
	env_vacpos(Env, xy(1,S)),
	env_vacdir(Env, east),
	random_locations(S, FProb, [xy(1,S)], Furniture),
	env_furniture(Env,Furniture),
	random_locations(S, DProb, [xy(1,S)|Furniture], Dirt),
	env_dirt(Env, Dirt),
	env_power(Env, on).

%---------------------------------------------------------------------------
% Support predicates for generating environments.

random_locations(Size, Prob, Avoid, Locations) :-
	findall(L, (random_location(Size, Prob, L),
                    \+ member(L,Avoid)),
                 Locations).

random_location(Size, Prob, xy(X,Y)) :-
	between(1, Size, X),
	between(1, Size, Y),
	Rand is random(100),
	Rand < Prob.

%---------------------------------------------------------------------------
% Successor predicate for VC world searching.
%     left, forward, and off need to be completed.

succ(Env0, suck, Env1) :-
	env_power(Env0, on),
	env_dirt(Env0, DL0),
	env_vacpos(Env0, Pos),
	del_one(Pos, DL0, DL1),
	env_set_dirt(Env0, DL1, Env1).

succ(Env0, right, Env1) :-
	env_power(Env0, on),
	env_vacdir(Env0, Dir0),
	clockwise_next(Dir0, Dir1),
	env_set_vacdir(Env0, Dir1, Env1).

succ(Env0, left, Env) :-
	env_power(Env0, on),
	env_vacdir(Env0, Dir0),
	clockwise_next(Dir1, Dir0),
	env_set_vacdir(Env0, Dir1, Env).

succ(Env0, forward, Env) :-
	env_power(Env0, on),
	env_vacpos(Env0, P0),
	env_vacdir(Env0, D),
	next_position(D, P0, P1),
	legal_position(Env0, P1),
	env_set_vacpos(Env0, P1, Env).

succ(Env0, off, Env):-
	env_power(Env0, on),
	env_set_power(Env0, off, Env).


%---------------------------------------------------------------------------
% Support predicates for successor.

clockwise_next(north, east).
clockwise_next(east, south).
clockwise_next(south, west).
clockwise_next(west, north).

del_one(_, [], []) :- !.
del_one(X, [X|Y], Y) :- !.
del_one(X, [H|T], [H|T1]) :-
	del_one(X,T,T1).

%next_position(+direction, +oldpos, ?newpos)
next_position(north, xy(X,Y), xy(X,Y1)) :- Y1 is Y - 1.
next_position(south, xy(X,Y), xy(X,Y1)) :- Y1 is Y + 1.
next_position(east, xy(X,Y), xy(X1,Y)) :- X1 is X + 1.
next_position(west, xy(X,Y), xy(X1,Y)) :- X1 is X - 1.

legal_position(Env, xy(X,Y)):-
	env_size(Env, Size),
	between(1, Size, X),
	between(1, Size, Y),
	env_furniture(Env, Furn),
	not(member(xy(X,Y), Furn)).

%------------------------------------------------------------
% predicate to display an environment

show(vw(Size, Furn, Pos, Direction, Dirt, _)):-
	between(1, Size, Y),
	nl,
	between(1, Size, X),
	choose_char(X,Y, Furn, Pos, Direction, Dirt, Char),
	write(Char),
	fail.
show(_) :- nl.

choose_char(X,Y, _, xy(X,Y), east, _, '>'):- !.
choose_char(X,Y, _, xy(X,Y), north, _, '^') :- !.
choose_char(X,Y, _, xy(X,Y), west, _, '<') :- !.
choose_char(X,Y, _, xy(X,Y), south, _, 'v') :- !.
choose_char(X,Y, Furn, _, _, _, 'F') :- member(xy(X,Y), Furn), !.
choose_char(X,Y, _, _, _, Dirt, 'D') :- member(xy(X,Y), Dirt), !.
choose_char(_, _, _, _, _, _, '.').

horizontal_line(0) :- nl, !.
horizontal_line(N) :-
	write('-'),
	N1 is N - 1,
	horizontal_line(N1).

%---------------------------------------------------------------------------
%Test environments

env(1, vw(2, [], xy(1, 2), east, [xy(2, 1)], on)).
env(2, vw(2, [xy(1, 1)], xy(1, 2), east, [xy(2, 2)], on)).


% loop to keep asking agent for perform actions until vacuum turns off.
run_agent(Env):-
	env_power(Env, off),
	!,
	writeln(done).
run_agent(Env):-
	show(Env),
	get0(_),
	%write('PERCEPT:'),
	%writeln(Env),
	agent:act(Env, Action),
	write('ACTION:'),
	writeln(Action),
	do_action(Env, Action, Env1),
	!,
	run_agent(Env1).

do_action(Env, Action, Env1):-
	succ(Env, Action, Env1), !.
do_action(Env, _, Env). % illegal action does nothing

