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

:- begin_clingo(color).

1 {color(X,I) : @color(I)} 1 :- @country(X).
:- color(X,I), color(Y,I), @border(X,Y).

:- end_clingo.

map_color(M) :-
	clingo_model(color, M).

:- begin_clingo(const).

#const x = 100.
p(x).

:- end_clingo.

const(M) :-
	clingo_model(const, M).

:- begin_clingo(show).

p(x).
q(y).

#show q/1.

:- end_clingo.

show(M) :-
	clingo_model(show, M).
