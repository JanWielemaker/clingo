:- use_module(library(clingo)).

param(M) :-
	clingo_new(C, []),
	clingo_add(C, prog(x),
		   "p(x)."),
	clingo_ground(C,
		      [ prog(4)
		      ]),
	clingo_solve(C, M).
