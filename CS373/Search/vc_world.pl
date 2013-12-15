% vc_sim.pl: Vacuum Cleaner World Simulator
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

room_size(8).       
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

% env_set_vacpos(E, xy(1,5), E1)
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
	env_power(Env0, on),              % checking to see if vacuum is on
	env_dirt(Env0, DL0),              % list of dirty cells
	env_vacpos(Env0, Pos),            % where the vacuum is
	del_one(Pos, DL0, DL1),           % delete dirt in cell if dirt exists
	env_set_dirt(Env0, DL1, Env1).    % sets the dirty cells with the new dirt list

succ(Env0, right, Env1) :-
	env_power(Env0, on),              % checking to see if vacuum is on
	env_vacdir(Env0, Dir0),           % determines vacuum direction
	clockwise_next(Dir0, Dir1),       % gets vacuum direction one step clockwise
	env_set_vacdir(Env0, Dir1, Env1). % sets the new direction

succ(Env0, left, Env1) :-
	env_power(Env0, on),
	env_vacdir(Env0, Dir0),
	clockwise_next(Dir1, Dir0),
	env_set_vacdir(Env0, Dir1, Env1).

succ(Env0, forward, Env1) :-
	env_power(Env0, on),
	env_vacdir(Env0, Dir0),
	forward_next(Pos0, Dir, Env0, Pos1),
	env_setvacpos(Env0, Pos1, Env1).

succ(Env0, off, Env1):-
	env_power(Env0,on),
	env_set_power(Env0, off, Env1).
	

%---------------------------------------------------------------------------
% Support predicates for successor.

clockwise_next(north, east).
clockwise_next(east, south).
clockwise_next(south, west).
clockwise_next(west, north).

forward_next(xy(X,Y0), north, Env0, xy(X,Y1):-
	     Y1 is Y0 - 1,                        % 
	     env_size(Env0,Size),                 % store env size as Size
	     Y1 =< Size,                          % Y1 has to be in the Env
	     env_furniture(Env0, FurnitureList),  % Get FurnitureList
	     \+ member(xy(X,Y1), FurnitureList),  % Check to see if there is furniture in the new spot
	     !.                                   % cut

forward_next(xy(X,Y0), south, Env0, xy(X,Y1):-
	     Y1 is Y0 - 1,                        % 
	     Y1 > 0,                              % location has to be in the Env
	     env_furniture(Env0, FurnitureList),  % get FurnitureList
	     \+ member(xy(X,Y1), FurnitureList),  % check to see if there is furniture in the new spot
	     !.                                   % cut
	    
forward_next(xy(X0,Y), east, Env0, xy(X1,Y):-
	     X1 is X0 - 1,                        % 
	     env_size(Env0,Size),                 % store env size as Size
	     X1 =< Size,                          % X1 has to be in the Env
	     env_furniture(Env0, FurnitureList),  % get FurnitureList
	     \+ member(xy(X1,Y), FurnitureList),  % check to see if there is furniture in the new spot
	     !.                                   % cut
	    
forward_next(xy(X0,Y), west, Env0, xy(X1,Y):-
	     X1 is X0 - 1,                        % 
	     X1 > 0,                              % X1 has to be in the Env
	     env_furniture(Env0, FurnitureList),  % get FurnitureList
	     \+ member(xy(X1,Y), FurnitureList),  % check to see if there is furniture in the new spot
	     !.                                   % cut

	    
del_one(_, [], []) :- !.
del_one(X, [X|Y], Y) :- !.
del_one(X, [H|T], [H|T1]) :-
	del_one(X,T,T1).

%---------------------------------------------------------------------------
% The solution predicate for VC World searches.
%      Needs to be supplied...


%----------------------------------------------------------------------------
% display the environment

show(vw(Size, Furn, Pos, Direction, Dirt, _)):-
	between(1, Size, Y),
	nl,
	between(1, Size, X),
	choose_char(X,Y, Furn, Pos, Direction, Dirt, Char),
	write(Char),
	fail.
show(_) :- nl,nl.

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
