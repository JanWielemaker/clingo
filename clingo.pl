:- module(clingo,
	  [ clingo_new/2,		% -Clingo, +Options
	    clingo_add/3,		% +Clingo, +ProgAndParams, +String
	    clingo_ground/2,		% +Clingo, +Options
	    clingo_solve/2,		% +Clingo, -Model
	    clingo_solve/3,		% +Clingo, +Assumptions, -Model
	    clingo_assign_external/3,	% +Clingo, +Atom, ?Value
	    clingo_release_external/2,	% +Clingo, +Atom

	    op(100, xfx, {})
	  ]).

:- use_foreign_library(clingo).

inject_values(Name, [Arity], Goal) :-
	length(Args, Arity),
	Goal =.. [Name|Args],
	call(Goal).

%%	clingo_solve(+Clingo, -Model)

clingo_solve(Clingo, Model) :-
	clingo_solve(Clingo, [], Model).
