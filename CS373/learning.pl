% Learning how to use prolog!

sunny.

goingToTheTournament(dan,no).

eats(fred, What).

loves(john,mary).
loves(fred,hobbies).

tape(1,van_morrison,astral_weeks,madam_george).

tape(2,beatles,sgt_pepper,a_day_in_the_life).

tape(3,beatles,abbey_road,something).

tape(4,rolling_stones,sticky_fingers,brown_sugar).

tape(5,eagles,hotel_california,new_kid_in_town).

mortal(X):-

	human(X).

human(socrates).

fun(X):-
	red(X),
	car(X).

fun(X):-
	blue(X),
	bike(X).

fun(ice_cream).

car(vw_beatle).
car(ford_escort).
bike(harley_davidson).
red(vw_beatle).
red(ford_escort).
blue(harley_davidson).