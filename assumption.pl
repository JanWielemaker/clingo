:- use_module(clingo).

assumption(M) :-
	clingo_new(C, []),
	clingo_add(C, prog(x),
		   "{p(1); p(x)}."),
	clingo_ground(C,
		      [ prog(4)
		      ]),
	clingo_solve(C,
		     [ ~(p(4))
		     ], M).
