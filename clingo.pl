/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Roland Kaminski
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(clingo,
	  [ clingo_new/2,		% -Clingo, +Options
	    clingo_add/3,		% +Clingo, +ProgAndParams, +String
	    clingo_load/2,		% +Clingo, +File
	    clingo_load/3,		% +Clingo, +File, +Parameters
	    clingo_ground/2,		% +Clingo, +Parameters
	    clingo_parse/2,		% +String, -AST
	    clingo_solve/2,		% +Clingo, -Model
	    clingo_solve/3,		% +Clingo, +Assumptions, -Model
	    clingo_solve/4,		% +Clingo, +Assumptions, +Show, -Model
	    clingo_assign_external/3,	% +Clingo, +Atom, ?Value
	    clingo_release_external/2,	% +Clingo, +Atom

	    clingo_model/2,		% +Clingo, -Model

	    begin_clingo/1,		% +ProgAndParams
	    end_clingo/0,

	    op(100, xfx, {}),
	    op(100, fx, #),
	    op(100, fx, ~),
	    op(200, fy, @),
	    op(1100, xfx, const),
	    op(1100, xfx, show),
	    op(1100, xfx, external)
	  ]).
:- use_foreign_library(foreign(clingo)).
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(apply_macros), []).

:- meta_predicate
	clingo_model(:,-).

/** <module> Clingo ASP interface

@author Roland Kaminski and Jan Wielemaker
*/

% Although the interface is licensed under the BSD license, the
% loaded Clingo library is covered by the GPL
:- license(gpl).

:- public inject_values/3.

inject_values(Name, [Arity, ModuleS], Goal) :- !,
	length(Args, Arity),
	Goal =.. [Name|Args],
	atom_string(Module, ModuleS),
	call(Module:Goal).
inject_values(Name, [Arity], Goal) :-
	length(Args, Arity),
	Goal =.. [Name|Args],
	call(Goal).

%%	clingo_solve(+Clingo, -Model)

clingo_solve(Clingo, Model) :-
	clingo_solve(Clingo, [], [shown], Model).

%%	clingo_solve(+Clingo, Assumptions, -Model)

clingo_solve(Clingo, Assumptions, Model) :-
	clingo_solve(Clingo, Assumptions, [shown], Model).

%%	clingo_load(+Clingo, +File) is det.
%%	clingo_load(+Clingo, +File, +Params) is det.

clingo_load(Clingo, File) :-
	clingo_load(Clingo, File, []).
clingo_load(Clingo, File, Params) :-
	absolute_file_name(File, Path,
			   [ extensions(['', lp]),
			     access(read)
			   ]),
	read_file_to_string(Path, Program, []),
	ParamTerm =.. [Path|Params],
	clingo_add(Clingo, ParamTerm, Program).


%%	clingo_model(:Program, -Model) is nondet.
%
%	True if Model is a model for Program.

clingo_model(Module:Program, Model) :-
	generalize(Program, Params),
	Module:clingo_program(Params, Source),
	clingo_new(Clingo, []),
	clingo_add(Clingo, Params, Source),
	clingo_ground(Clingo, [ Program ]),
	clingo_solve(Clingo, Model).

generalize(Term, General) :-
	compound(Term), !,
	compound_name_arity(Term, Name, Arity),
	compound_name_arity(General, Name, Arity).
generalize(Term, Term).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

%%	begin_clingo(+Params) is det.
%
%	Begin an embedded Clingo program.

begin_clingo(Params) :-
	throw(error(context_error(nodirective, begin_clingo(Params)), _)).

%%	end_clingo is det.
%
%	End an embedded Clingo program

end_clingo :-
	throw(error(context_error(nodirective, end_clingo), _)).

:- thread_local
	clingo_term/1.

compile_clingo([begin(Params)|Terms],
	       [ (:-multifile(clingo_program/2)),
		 clingo_program(Params, Program)
	       ]) :-
	phrase(clingo_program(Terms), ProgramCodes),
	string_codes(Program, ProgramCodes).

clingo_program([]) --> [].
clingo_program([H|T]) -->
	{ debug(clingo(compile), 'Translate ~q', [H])
	},
	clingo_statement(H),
	clingo_program(T).

clingo_statement(Head :- Body) --> !,
	clingo_head(Head), " :- ",
	clingo_body(Body), ".\n".
clingo_statement(:- Body) --> !,
	":- ", clingo_body(Body), ".\n".
clingo_statement(#const Name = Value) --> !,
	"#const ", clingo_id(Name), " = ", term(Value), ".\n".
clingo_statement(#external Ext) --> !,
	"#external ", external(Ext), ".\n".
clingo_statement(#show Show) --> !,
	"#show ", show(Show), ".\n".
clingo_statement(Head) -->
	clingo_head(Head), ".\n".

clingo_head(Low {Alts} High) --> !,
	integer(Low), " {", clingo_alts(Alts), "} ", integer(High).
clingo_head(Term) -->
	term(Term).

clingo_alts((A;B)) --> !,
	clingo_alts(A), ";", clingo_alts(B).
clingo_alts(A:B) --> !,
	term(A),":",clingo_body(B).
clingo_alts(A) -->
	term(A).

clingo_body((A,B)) --> !,
	clingo_body(A),
	",\n",
	clingo_body(B).
clingo_body(@Goal) --> !,
	{ compound_name_arity(Goal, Name, Arity) },
	term(Goal), " = @", clingo_id(Name),
	"(", integer(Arity), src_module, ")".
clingo_body(A = B) --> !,
	term(A), " = ", term(B).
clingo_body(A \= B) --> !,
	term(A), " != ", term(B).
clingo_body(A) -->
	"    ", term(A).

src_module -->
	{ prolog_load_context(module, Module) }, !,
	", ",
	(   { is_valid_clingo_id(Module) }
	->  atom(Module)
	;   { atom_codes(Module, Codes) }
	->  clingo_string(Codes)
	).
src_module -->
	"".

external((A,B)) --> !,
	external(A), ", ",
	external(B).
external(A) -->
	term(A).

show((A,B)) --> !,
	show(A), ", ",
	show(B).
show(A) -->
	term(A).

clingo_id(Name) -->
	{ valid_clingo_id(Name) },
	atom(Name).

arguments([]) --> [].
arguments([H|T]) -->
	term(H),
	(   {T==[]}
	->  ""
	;   ", ",
	    arguments(T)
	).

term(Var) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
term(Num) -->
	{ integer(Num),
	  must_be(between(-2147483648, 2147483647), Num)
	}, !,
	integer(Num).
term(String) -->
	{ string(String), !,
	  string_codes(String, Codes)
	},
	clingo_string(Codes).
term(Atom) -->
	{ atom(Atom) },
	clingo_id(Atom).
term('$VAR'(Var)) -->
	clingo_var(Var).
term(#Keyword) --> !,
	{ clingo_keyword(Keyword) },
	"#", atom(Keyword).
term(Name/Arity) --> !,
	{ must_be(nonneg, Arity) },
	clingo_id(Name), "/", integer(Arity).
term(Term) -->
	{ compound(Term), !,
	  compound_name_arguments(Term, Name, Arguments)
	},
	clingo_id(Name), "(", arguments(Arguments), ")".

%%	valid_clingo_id(+Atom) is det.
%
%	@error domain_error(clingo_id, Atom).

valid_clingo_id(Atom) :-
	is_valid_clingo_id(Atom), !.
valid_clingo_id(Atom) :-
	domain_error(clingo_id, Atom).

is_valid_clingo_id(Atom)  :-
	atom_codes(Atom, Codes),
	maplist(between(1,127), Codes),
	Codes = [H|T],
	(   code_type(H, lower)
	->  true
	;   H == 0'_
	),
	maplist([C]>>code_type(C, csym), T), !.

clingo_var(Name, List, Tail) :-
	format(codes(List,Tail), '~p', ['$VAR'(Name)]).

clingo_string(Codes) --> "\"", clingo_string_codes(Codes), "\"".
clingo_string_codes([]) --> "".
clingo_string_codes([H|T]) -->
	clingo_string_code(H),
	clingo_string_codes(T).

clingo_string_code(0'\") --> !, "\\\"".
clingo_string_code(0'\n) --> !, "\\n".
clingo_string_code(C) --> [C].

clingo_keyword(Kwd) :-
	is_clingo_keyword(Kwd), !.
clingo_keyword(Kwd) :-
	domain_error(clingo_keyword, Kwd).

is_clingo_keyword(0) :- !, fail.
is_clingo_keyword(inf).
is_clingo_keyword(sup).
is_clingo_keyword(external).
is_clingo_keyword(show).
is_clingo_keyword(const).


		 /*******************************
		 *	  TERM EXPANSION	*
		 *******************************/

clingo_expand((:- begin_clingo(Params)), []) :-
	(   clingo_term(_)
	->  syntax_error('nested clingo program')
	;   assertz(clingo_term(begin(Params)))
	).
clingo_expand((:- end_clingo), Clauses) :-
	findall(Term, retract(clingo_term(Term)), Terms),
	(   Terms = [begin(Params)|Terms0]
	->  maplist(bind_vars, Terms0, Terms1),
	    compile_clingo([begin(Params)|Terms1], Clauses)
	;   syntax_error('unexpected :- clingo_end')
	).
clingo_expand(Term, []) :-
	prolog_load_context(variable_names, Bindings),
	clingo_term(_), !,
	assertz(clingo_term(t(Term, Bindings))).

bind_vars(t(Term, Bindings), Term) :-
	maplist(bind_var, Bindings).

bind_var(Name = Var) :-
	Var = '$VAR'(Name).

system:term_expansion(In, Out) :-
	clingo_expand(In, Out).


		 /*******************************
		 *	      SANDBOX		*
		 *******************************/

sandbox:safe_primitive(clingo:clingo_new(_,_)).
sandbox:safe_primitive(clingo:clingo_add(_,_,_)).
sandbox:safe_primitive(clingo:clingo_ground(_,_)).
sandbox:safe_primitive(clingo:clingo_solve(_,_)).
