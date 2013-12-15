:- module(alists, [lookup/2, modify/3]).

%---------------------------------------------------------------------------
%    Association lists.

%    There is a built in module library(assoc) for managing
%    association lists using AVL trees. For large lists, that would be
%    the best option. For the smaller lists with specialized
%    operations here, it is simpler to use the lists form directly.

%    main methods:
%       lookup(AList, [Key1-Value1, Key2-Value2,...])
%              Look up the Keys in AList and match then with Values
%
%       modify(Alist0, [Key1-Value1, Key2-Value2,...], Alist)
%            Alist is Alist0 with the given modified to have the given
%            values
%
%       Note: Keys MUST BE GROUND (contain no variables) and
%       UNIQUE. Both methods throw an error if an attempt is made to
%       access a key that does not exist in the list.

lookup(Env, Pairs):- lookup_loop(Pairs, Env).

lookup_loop([Key-Value|Pairs], Env):-
	alist_get(Env, Key, Value),
	lookup_loop(Pairs, Env).
lookup_loop([], _).
lookup_loop(Key-Value, Env) :- alist_get(Env, Key, Value). % single value lookup

alist_get([Key-Value|_], Key, Value1) :- !, Value = Value1.
alist_get([_|Pairs], Key, Value) :- alist_get(Pairs, Key, Value),!.
alist_get([], Key, _):- throw(non_existent_key(Key)).


modify(Env0, Pairs, Env) :-
	modify_loop(Pairs, Env0, Env).

modify_loop([Key-Value|Pairs], Env0, Env) :-
	alist_set(Env0, Key, Value, Env1),
	modify_loop(Pairs, Env1, Env).
modify_loop([], E, E).
modify_loop(Key-Value, AL0, AL) :- alist_set(AL0, Key, Value, AL).

alist_set([Key-_|Pairs], Key, Value, [Key-Value|Pairs]).
alist_set([Pair|Pairs0], Key, Value, [Pair|Pairs]) :-
	alist_set(Pairs0, Key, Value, Pairs).
alist_set([], Key, _, _):- throw(non_existent_key(Key)).

