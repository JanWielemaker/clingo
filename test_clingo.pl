:- module(test_clingo,
	  [ test_clingo/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(clingo)).

test_clingo :-
	run_tests([ symbolic_atoms
		  ]).


:- begin_tests(symbolic_atoms).

test(fact, Info == symbol(a,fact)) :-
	ground(Clingo, "a."),
	clingo_symbol_lookup(Clingo, a, Info).
test(fact, Info == symbol(a,unknown)) :-
	ground(Clingo, "{a}."),
	clingo_symbol_lookup(Clingo, a, Info).
test(fact, Info == symbol(a,external)) :-
	ground(Clingo, "#external a."),
	clingo_symbol_lookup(Clingo, a, Info).

:- end_tests(symbolic_atoms).

ground(Clingo, Program) :-
	clingo_new(Clingo, []),
	clingo_add(Clingo, base, Program),
	clingo_ground(Clingo, [base]).
