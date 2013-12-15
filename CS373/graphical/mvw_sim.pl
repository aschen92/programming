% mvw_sim.pl: Multiple Vacuum World Simulator
% Programmer: John Zelle

/* Both the rules for the simulation and the current state of
   the environment are represented using an association list. This
   is a set of key-value pairs similar to a Python dictionary. 
*/

:-use_module(alists).

% "Rules" for the simulation. This gives basic values, but does not include
%   scoring.

rules([aux_battery-true,   % Whether an agent can move after charge reaches 0
       room_size-5,       % Environment is square with this many cells on a side
       furniture_prob-20,  % Probability that a given square has furniture
       dirt_prob-25,       % Probability that a given square is dirty
       charger_prob-5,     % Probability that a given square has a outlet 
       full_capacity-2,   % The amount of dirt an empty vacuum can suck up
       full_charge-20 ]).  % The number of moves available before recharge
	


% Environment
% Keys:
%    size -- size of the environment
%    pos(N), dir(N), capacity(N), charge(N), home(N) -- values of Nth agent
%    dirt -- dirtlist
%    furniture -- furniture list
%    chargers -- charging station list
%    max_charge -- full charge amount
%    max_capacity -- full capacity amount

% e.g.

environment(1, [size-5,
		pos(1)-xy(1,5), dir(1)-east,
		capacity(1)-10, charge(1)-20, home(1)-xy(1,5),
		pos(2)-xy(5,1), dir(2)-west,
		capacity(2)-10, charge(2)-20, home(2)-xy(5,1),
		furniture-[xy(3,3), xy(2,2)],
		dirt-[],
	        chargers-[]
	       ]
	   ).

%---------------------------------------------------------------------------
% Execute action in the environment to produce new environment
%   actions: forward, left, right, suck, charge, dump

% update_env(Action, AgentNum, EnvBefore, EnvAfter)
%     AgentNum is 1 or 2 to indicate which agent is performing the action.
%     This predicate always succeeds. If the requested action is not
%        legal, the environment remains unchanged, except for costing
%        the agent a unit of charge.

update_env(A, I, Env0, Env):-
	attempt_action(A, I, Env0, Env1),
	decrement_charge(Env1, I, Env),
	!.
update_env(_,I,Env0, Env):- % if action fails, just update charge.
	decrement_charge(Env0, I, Env).

decrement_charge(E0, I, Env) :-
	lookup(E0, [charge(I)-C0]),
	C is C0 - 1,
	modify(E0, [charge(I)-C], Env).


% attempt_action(Action, AgentNum, EnvBefore, EnvAfter)
%      This is essentially the successor predicate. AgetNum tells which
%      agent is acting.  Attempt to perform action, fail if Action
%      is illegal

attempt_action(suck, I, Env0, Env) :-
	lookup(Env0,
	       [charge(I)-Charge0,
		capacity(I)-Cap0,
		pos(I)-Pos,
		dirt-DL0]),
	Charge0 > 0,
	Cap0 > 0,
	del_one(Pos, DL0, DL1),
	Cap1 is Cap0 - 1,
	modify(Env0,
	       [capacity(I)-Cap1, dirt-DL1],
	       Env).

attempt_action(forward, I, Env0, Env) :-
	OtherI is 3-I,
 	lookup(Env0,  [size-S,
		       pos(I)-Pos0,
		       dir(I)-Dir0,
		       charge(I)-Charge0,
		       pos(OtherI)- OppPos,
		       furniture-Furniture]),
	can_move(Charge0),
	next_position(Dir0, Pos0, Pos1),
	legal_position(S, Pos1),
	Pos1 \= OppPos,
	\+ member(Pos1, Furniture),
	modify(Env0, [pos(I)-Pos1], Env).

attempt_action(right, I, Env0, Env) :-
	lookup(Env0, [dir(I)-Dir0, charge(I)-Charge0]),
	can_move(Charge0),
	clockwise_next(Dir0, Dir1),
	modify(Env0, [dir(I)-Dir1], Env).

attempt_action(left, I, Env0, Env) :-
	lookup(Env0, [dir(I)-Dir0, charge(I)-Charge0]),
	can_move(Charge0),
	clockwise_next(Dir1, Dir0),
	modify(Env0, [dir(I)-Dir1], Env).

attempt_action(dump, I, Env0, Env) :-
	lookup(Env0, [home(I)-Pos, pos(I)-Pos, charge(I)-Charge0]),
	can_move(Charge0),
	rules(Rs),
	lookup(Rs, [full_capacity-C]),
	modify(Env0, [capacity(I)-C], Env).

attempt_action(charge, I, Env0, Env) :-
	lookup(Env0, [pos(I)-Pos, chargers-Chargers, home(I)-Home]),
	(memberchk(Pos, Chargers); Pos = Home),
	rules(Rs),
	lookup(Rs, [full_charge-Full]),
	modify(Env0, [charge(I)-Full], Env).

%---------------------------------------------------------------------------
% Helper predicates for performing actions.

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

% %---------------------------------------------------------------------------
% Generate a random starting environment based on the rules in rules/1.

random_env(Env) :-
	rules(Rules),
	lookup(Rules, [room_size-S, furniture_prob-FProb,
	       charger_prob-CProb, dirt_prob-DProb, full_capacity-Cap,
	       full_charge-Charge]),
 	random_locations(S, FProb, [xy(1,S), xy(S,1)], Furniture),
 	random_locations(S, CProb, [xy(1,S), xy(S,1)|Furniture], Chargers),
	random_locations(S, DProb, [xy(1,S), xy(S,1)|Furniture], Dirt),
 	Env = [size-S,
	       capacity(1)-Cap, charge(1)-Charge, home(1)-xy(1,S),
	       pos(2)-xy(S,1), dir(2)-west,
	       capacity(2)-Cap, charge(2)-Charge, home(2)-xy(S,1),
	       furniture-Furniture,
	       dirt- Dirt,
	       pos(1)-xy(1,S), dir(1)-east,
	       chargers-Chargers,
	       max_charge-Charge,
	       max_capacity-Cap].
	       
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
% ASCII display of environment

show(Env):-
	lookup(Env, [size-Size]),
 	between(1, Size, Y),
	nl,
 	%L is Size * 2,
	%horizontal_line(L),
	write(' '),
 	between(1, Size, X),
	%lookup(Env, [pos(I)-Loc]),
	%write(Loc),nl,
 	choose_char(xy(X,Y),  Env, Char),
 	write(Char), write(' '),
 	fail.
show(_) :- nl, nl.

choose_char(Loc, Env, 'F') :-
	lookup(Env, [furniture-Furn]),
	memberchk(Loc, Furn), !.
choose_char(Loc, Env, 'D') :-
	lookup(Env, [dirt-Dirt]),
	memberchk(Loc, Dirt), !.
choose_char(Loc, Env, 'C') :-
	lookup(Env, [chargers-Cs]),
	memberchk(Loc, Cs), !.
choose_char(Loc, Env, DChar):-
   	(lookup(Env, [pos(1)-Loc, dir(1)-Dir]);
	 lookup(Env, [pos(2)-Loc, dir(2)-Dir])
	),
   	direction_char(Dir, DChar),
	!.
choose_char(_, _, '.').

direction_char(east, '>').
direction_char(north, '^').
direction_char(west, '<').
direction_char(south, 'v').

%---------------------------------------------------------------------------
%  Simulate actions for one agent in the environment

run_agent(Env, I):-
	set_id(I),
	run_loop(Env, I).

run_loop(E, I):-
 	simulation_over(E, I),
 	!,
 	writeln(done).
run_loop(Env, I):-
 	show(Env),
	lookup(Env, [charge(I)-Charge, capacity(I)-Cap]),
	write('Capacity: '), write(Cap),
	write('    Charge: '), writeln(Charge),
 	get0(_),                % pause for keyboard input
	%write('PERCEPT:'),
	%writeln(Env),
 	agent:act(Env, Action),
 	write('ACTION:'),
 	writeln(Action),
 	update_env(Action, I, Env, Env1),
 	!,
 	run_loop(Env1, I).
	

simulation_over(E, I) :-
	rules(Rs),
	\+ points_left(E, I, Rs).
	
points_left(E, I, Rs):-
	alive(E, I, Rs),
	dirt_available(E, I, Rs).

alive(_,_,Rs) :-
	lookup(Rs, [aux_battery-true]), !.
alive(E,I,_) :-
	lookup(E, [charge(I)-Charge]),
	Charge > 0.

dirt_available(E,_,_) :-
	lookup(E, [dirt-[_|_]]), !.      % can suck some dirt up
dirt_available(E, I, Rs) :-              % have some dirt to dump
	lookup(E, [capacity(I)-Cap]),
	lookup(Rs, [full_capacity-FCap]),
	Cap < FCap.
