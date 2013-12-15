% FILE: Time.pl
% DESCRIPTION:  Some predicates for timing programs.

% Example use: 
%     ?- start(S), cpu_time(bf_search(S,F,Ops), Time).

% cpu_time(-Time) -- gets number of seconds of runtime since start
%   of the prolog process

cpu_time(Time) :-
	statistics(cputime,Time).

% cpu_time(+Goal, -Duration) -- Returns the user run time of executing
%    the goal.

cpu_time(Goal, Duration) :-
	statistics(cputime, Start),
	( call(Goal) -> true; true ),
	statistics(cputime, Finish),
	Duration is (Finish - Start).


% cpu_time(+N, +Goal, -Duration) -- Returns the user run time required
%   to execute the goal N times.

cpu_time(N, Goal, Duration) :-
	statistics(cputime,T0),
	( call(( repeat(N), (Goal -> fail) )); true ),
	statistics(cputime,T1),
	( call(( repeat(N), (true -> fail) )) ; true),
	statistics(cputime, T2),
	Duration is ((T1-T0) - (T2-T1)).

repeat(N) :-
	N > 0.
repeat(N) :-
	N > 1,
	N1 is N - 1,
	repeat(N1).


