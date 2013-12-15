% Shrink.pl: A simple shell for an Eliza-like dialogue program.

run:-
        write('Welcome to my sofa'),
        nl, nl,
        repeat,
        getsentence(Sent),
        pick_response(Sent, Response),
        write_list(Response), nl,
        Response = [goodbye].

%---------------------------------------------------------------------------
getsentence(Words) :-
        get0(Ch),
        getrest(Ch, Words).

getrest(10, []) :- !.
getrest(32, Words) :-
        !,
        getsentence(Words).
getrest(Ch, [W|Ws]) :-
        getword(Ch, Chs, Next),
        name(W, Chs),
        getrest(Next, Ws).

getword(10, [], 10) :- !.
getword(32, [], 32) :- !.
getword(Ch, [Ch|Chs],Next) :-
        get0(Ch1),
        getword(Ch1,Chs,Next).

%---------------------------------------------------------------------------
response([bye],[goodbye]).

response(Sent,[i,am,glad,you,are,expressing,yourself,but,please,watch,the,vulgar,language]):-  % Added
	member(shit,Sent),
	!.
response(Sent,[i,am,glad,you,are,expressing,yourself,but,please,watch,the,vulgar,language]):-   % Added
	member(bitch,Sent),
	!.
response(Sent,[i,am,glad,you,are,expressing,yourself,but,please,watch,the,vulgar,language]):-  % Added
	member(ass,Sent),
	!.
response(Sent,[i,am,glad,you,are,expressing,yourself,but,please,watch,the,vulgar,language]):-  % Added
	member(fuck,Sent),
	!.
response(Sent,[i,am,glad,you,are,expressing,yourself,but,please,watch,the,vulgar,language]):-   % Added
	member(damn,Sent),
	!.

response([hello], [how,can,i,help,you]).
response([i, am | X],[please, tell, me, Wword, you, are | X1]) :-
        getWword(Wword),
        spit(X,X1).
response([i,have|X],[how,long,have,you,had|X1]) :-
        spit(X,X1).
response([i,feel|_],[i,sometimes,feel,the,same,way]).
response([because|_],[is,that,really,the,reason]).
response([],[please,say,something]).
response([yes|X],[how,can,you,be,so,sure |X1]) :-
        spit(X,X1).
response([yes|[X|Xs]],[you,seem,certain,that |X1]) :-
        spit([X|Xs],X1).
response([you, are|X],[oh,yeah,i,am|X1]) :-
        spit(X,X1).
response([Verb|X],[oy,yoi,yoi,you,want,that,i,should,go,and,Verb|X1]) :-
        verb(Verb),
        spit(X,X1).
response([Wword|_],[you,tell,me,Wword]) :-
        wWord(Wword).
response([Dword,you|X],[perhaps,i,Dword|X1]) :-
        dWord(Dword),
        spit(X,X1).
response([do,you,think|_],[i,think,you,should,answer,that,yourself]).
response(Sent,[for,dream,analysis,see,freud]) :-
        member(dream,Sent).
response(Sent,[all,is,fair,in,love,and,war]) :-
        member(love,Sent).
response(Sent,[dont,be,so,negative]) :-
        member(no,Sent).
response(Sent,[be,more,decisive]) :-
        member(maybe,Sent).
response([am,i|X], [do, you,think,you,are|X]).
response(Sent, [tell, me, more, about, your, W]) :-
        append(_,[my, W|Ws], Sent), 
        member(me, Ws).
response(Sent,Sent1) :-
        member(i,Sent),
        spit(Sent,Sent1).
% ---------------------------------------------
% Added Responses

response(Sent,[explain,to,me,why,you,feel,hatred]) :-
        member(hate,Sent).
response([],[i,can,not,help,you,if,you,do,not,let,me]).
response([what,do,you,want|_],[do,not,be,concerned,about,what,i,want,because,i,am,here,to,help,you]).
response([thank,you|_],[you,are,welcome,now,tell,me,what,is,on,your,mind]).


% ---------------------------------------------

verb(go). verb(have). verb(be). verb(try). verb(eat). verb(take).
verb(help). verb(make). verb(get). verb(jump). verb(write). verb(type).
verb(fill). verb(put). verb(turn). verb(compute). verb(calculate).
verb(think). verb(drink). verb(blink). verb(crash). verb(crunch). verb(add).

dWord(do). dWord(can). dWord(should). dWord(would).

wWord(why). wWord(where). wWord(when). wWord(what).

punt([please,go,on]).
punt([tell,me,more]).
punt([i,see]).
punt([what,does,that,indicate]).
punt([but,why,be,concerned,about,it]).
punt([just,tell,me,how,you,feel]).

punt([do,you,think,this,has,something,to,do,with,your,relationship,with,your,mother]). % Added
punt([and,how,does,that,make,you,feel]).                                               % Added
punt([if,you,had,to,describe,your,feelings,right,now,as,a,color,which,color,would,it,be]).  % Added

%---------------------------------------------------------------------------
%  Change These Predicates
%---------------------------------------------------------------------------
pick_response(Sent, R):-
	findall(R, response(Sent, R), Rs),
	length(Rs, Len),
	Rs \= [],
	Rand is random(Len),
	nth0(Rand, Rs, R),!.

pick_response(_,R) :-
	findall(R, punt(R), Rs),
	length(Rs, Len),
	Rand is random(Len),
	nth0(Rand, Rs, R), !.



%---------------------------------------------------------------------------
%spit(X,X).
%"i", "me", "you", "my", "your", "yours", "mine", "am". 

spit([],[]).
spit([H|T], [Fixed|T1]):-
	spit1(H,Fixed),
	spit(T,T1).


spit1(i,you) :- !.
spit1(you,i) :- !.
spit1(me, you) :- !.
spit1(your,my) :- !.
spit1(my,your) :- !.
spit1(yours, mine) :- !.
spit1(mine,yours) :- !.
spit1(am, are) :- !.
spit1(are,am) :- !.
spit1(X,X):- !.

%---------------------------------------------------------------------------
write_list([]):-
	nl.
write_list([H|T]) :-
        write(H),
	write(' '),
	write_list(T).


%---------------------------------------------------------------------------
getWword(W) :-
	findall(W, wWord(W), Ws),
	length(Ws,Len),
	Rand is random(Len),
	nth0(Rand, Ws, W).

%---------------------------------------------------------------------------