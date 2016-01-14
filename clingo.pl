:- module(clingo,
	  [ clingo_new/2,		% -Clingo, +Options
	    clingo_add/2,		% +Clingo, +String
	    clingo_ground/2,		% +Clingo, +Options
	    clingo_solve/2		% +Clingo, -Model
	  ]).

:- use_foreign_library(clingo).

inject_values(Name, [Arity], Goal) :-
	length(Args, Arity),
	Goal =.. [Name|Args],
	call(Goal).
