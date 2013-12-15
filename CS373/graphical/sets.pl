% sets.pl -- efficient sets using assoc library
% Programmer: John Zelle

:-module(sets, [set_empty/1, set_add/3, set_add_new/3, set_member/2,
		set_memberchk/2, set_add_list/3, list_to_set/2, set_to_list/2]).

:- use_module(library(assoc)).

%---------------------------------------------------------------------------
/*  
   Operations:
       set_empty(?Set) 
       set_add(+Set0, +Element, -Set)
       set_add_new(+Set0, +Element, -Set)
            Fails if Element is already in Set0
       set_member(-Element, +Set)
            used to enumerate the set
       set_memberchk(+Element, +Set)
       set_add_list(+Set0, +List, -Set)
       list_to_set(+List, -Set)
       set_to_list(+Set, -List)
*/
%---------------------------------------------------------------------------
set_empty(S) :- empty_assoc(S).

%---------------------------------------------------------------------------
set_add(Set0, Element, Set1) :- put_assoc(Element, Set0, t, Set1).

%---------------------------------------------------------------------------
set_add_new(Set0, Element, Set1) :-
	\+ get_assoc(Element, Set0, _),
	put_assoc(Element, Set0, t, Set1).

%---------------------------------------------------------------------------
set_member(Element, Set) :- gen_assoc(Element, Set, _).

%---------------------------------------------------------------------------
set_memberchk(Element, Set) :-
	get_assoc(Element, Set, _).

%---------------------------------------------------------------------------
set_add_list(Set0, Items, Set1) :-
	add_list_helper(Items, Set0, Set1).

add_list_helper([], Set, Set).
add_list_helper([H|T], Set0, Set) :-
	set_add(Set0, H, Set1),
	add_list_helper(T, Set1, Set).

%---------------------------------------------------------------------------
list_to_set(List, S) :-
	findall(Key-t, member(Key,List), Pairs),
	list_to_assoc(Pairs, S).

%---------------------------------------------------------------------------
set_to_list(Set, List) :- assoc_to_keys(Set, List).
