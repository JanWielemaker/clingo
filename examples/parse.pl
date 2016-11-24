:- use_module(library(clingo)).

parse(T) :-
	clingo_parse("p(1). p(x).", T).
