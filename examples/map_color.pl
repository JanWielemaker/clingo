:- use_module(library(clingo)).

color(red).
color(blue).
color(green).

% from http://www.dai.ed.ac.uk/groups/ssp/bookpages/quickprolog/node34.html
ngb(portugal, [spain]).
ngb(spain, [portugal,france]).
ngb(france, [spain,belgium,switzerland,w_germany,italy]).
ngb(belgium, [france,w_germany,netherlands]).
ngb(netherlands, [belgium,w_germany]).
ngb(w_germany, [netherlands,belgium,france,switzerland,austria,denmark]).
ngb(switzerland, [france,w_germany,austria,italy]).
ngb(austria, [w_germany,switzerland,italy]).
ngb(italy, [france,switzerland,austria]).
ngb(denmark, [w_germany]).

country(C) :-
	ngb(C, _).

border(A,B) :-
	ngb(A, Borders),
	member(B, Borders).

map_color(M) :-
	clingo_new(C, []),
	clingo_add(C, base,
		   "1 {color(X,I) : c(I)} 1 :- v(X).
		    :- color(X,I), color(Y,I), e(X,Y), c(I).
		    c(C) :- color(C) = @color(1).
		    v(X) :- country(X) = @country(1).
		    e(X,Y) :- border(X,Y) = @border(2).
		    #show color/2."),
	clingo_ground(C,
		      [ base
		      ]),
	clingo_solve(C, M).
