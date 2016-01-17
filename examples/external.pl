:- use_module(library(clingo)).
:- use_module(library(debug)).

external :-
	clingo_new(C, []),
	clingo_add(C, prog,
		   "#external p(a)."),
	clingo_ground(C, [prog]),
	clingo_assign_external(C, p(a), true),
	assertion((clingo_solve(C, M1), M1 == [p(a)])),
	clingo_assign_external(C, p(a), false),
	assertion((clingo_solve(C, M2), M2 == [])),
	clingo_assign_external(C, p(a), _),
	assertion((findall(M3, clingo_solve(C, M3), M3s),
		   M3s == [[], [p(a)]])).
