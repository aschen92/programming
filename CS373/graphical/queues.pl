%  queues.pl:  package implementing queues.
%  Programmer: John Zelle

:- module(queues, [queue_empty/1, dequeue/3, enqueue/3, dequeue_list/3,
		   enqueue_list/3, list_to_queue/2, queue_to_list/2,
		   queue_length/2]).

%---------------------------------------------------------------------------
/*
   Representation of queues is a difference list with a length indicated
   by unary notation (ala Richard O'Keefe's "The Craft of Prolog").
*/

%---------------------------------------------------------------------------
/*
   Operations:
       queue_empty(?Queue)
       dequeue(-Element,+Queue, -Queue)
       dequeue_list(-List, +Queue, -Queue)
       enqueue(+Element, +Queue, -Queue)
       enqueue_list(+List, +Queue, -Queue)
       list_queue(+List, -Queue)
       queue_length(+Queue, -Length)
*/

%---------------------------------------------------------------------------
queue_empty(q(0,B,B)).

%---------------------------------------------------------------------------
dequeue(X, q(s(N),[X|F],B), q(N,F,B)).

%---------------------------------------------------------------------------
enqueue(X, q(N,F,[X|B]), q(s(N),F,B)).

%---------------------------------------------------------------------------
enqueue_list([], Q, Q).
enqueue_list([X|Xs], Q0, Q) :-
	enqueue(X, Q0, Q1),
	enqueue_list(Xs, Q1, Q).

%---------------------------------------------------------------------------
dequeue_list([], Q, Q).
dequeue_list([X|Xs], Q0, Q) :-
	dequeue(X, Q0, Q1),
	dequeue_list(Xs, Q1, Q).

%---------------------------------------------------------------------------
queue_length(q(Count,_,_),L) :-
	queue_length(Count,0,L).

queue_length(0, L, L).
queue_length(s(N), L0, L) :-
	L1 is L0 + 1,
	queue_length(N, L1, L).

%---------------------------------------------------------------------------
list_to_queue(List, Q) :-
	queue_empty(Q0),
	enqueue_list(List, Q0, Q).

%---------------------------------------------------------------------------
queue_to_list(Q, List) :-
	queue_length(Q, N),
	length(List, N),
	dequeue_list(List, Q, _).