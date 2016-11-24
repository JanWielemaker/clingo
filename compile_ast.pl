:- module(ast,
	  [ test/0,
	    ast_interface/0,
	    translate_ast/0
	  ]).

:- op(200, yf, ?).
:- op(200, yf, *).
:- op(200, yf, +).

test :-
	setup_call_cleanup(
	    (	current_output(Old),
		open(pipe(less), write, Out),
		set_output(Out)
	    ),
	    translate_ast,
	    (	set_output(Old),
		close(Out)
	    )).

ast_interface :-
	setup_call_cleanup(
	    tell('ast_read.c'),
	    translate_ast,
	    told).

translate_ast :-
	read_ast('clingo.ast', Statements),
	phrase(ast_types(Statements), Types0),
	list_to_set(Types0, Types),
%	pp(Types),
	header,
	maplist(declare, Types),
	format('~n', []),
	maplist(translate, Types).

header :-
	format('#include <clingo.h>~n', []),
	format('#include <SWI-Prolog.h>~n', []),
	format('~n', []).


read_ast(File, AST) :-
	setup_call_cleanup(
	    set_prolog_flag(allow_variable_name_as_functor, true),
	    setup_call_cleanup(
		open(File, read, In),
		read_ast_stream(In, AST),
		close(In)),
	    set_prolog_flag(allow_variable_name_as_functor, false)).

read_ast_stream(In, List) :-
	read_term(In, H, [variable_names(Bindings), module(ast)]),
	(   H == end_of_file
	->  List = []
	;   List = [H|T],
	    maplist(call, Bindings),
	    read_ast_stream(In, T)
	).

%%	ast_types(+Statements)//

ast_types([]) --> [].
ast_types([H|T]) -->
	ast_type(H),
	ast_types(T).

ast_type((Name=Def1|Union)) --> !,
	{ phrase(bar_list((Def1|Union)), List) },
	[Name=List],
	union_types(List, Name).
ast_type(Name=Union) -->
	{ is_list(Union) }, !,
	[Name=Union],
	union_types(Union, Name).
ast_type(Name=Type) -->
	[Name=Type],
	subtypes(Type).

union_types([], _) --> [].
union_types([H|T], Name) -->
	[ union_of(Name, H) ],
	subtypes(H),
	union_types(T, Name).

subtypes(Struct) -->
	{ Struct =.. [_|Args] },
	argtypes(Args).

argtypes([]) --> [].
argtypes([(A:T1|TL)|T]) --> !,
	{ phrase(bar_list((T1|TL)), List) },
	ast_type(A=List),
	argtypes(T).
argtypes([_:Type|T]) -->
	arg_type(Type),
	argtypes(T).

arg_type(Type?) --> !,
	arg_type(Type).
arg_type(Type*) --> !,
	arg_type(Type).
arg_type(Type+) --> !,
	arg_type(Type).
arg_type(Dotted) -->
	{ functor(Dotted, ., 2) }, !.
arg_type(Type) -->
	{ compound(Type) }, !,
	[ Type ],
	subtypes(Type).
arg_type(_) -->
	[].

declare(Name=Union) :-
	is_list(Union),
	format('static int \c
	        unify_ast_~w(term_t t, const clingo_ast_~w_t *ast);~n',
	       [Name, Name]), !.
declare(Name=Struct) :-
	Struct =.. [_Type|_Args],
	format('static int \c
		unify_ast_~w(term_t t, const clingo_ast_~w_t *ast);~n',
	       [Name, Name]), !.
declare(Struct) :-
	Struct \= (_=_),
	struct_member_name(Struct, CType), !,
	declare(CType=Struct).
declare(Term) :-
	format('TODO: ~p~n', [Term]).


translate(Name=Union) :-
	is_list(Union),
	format('static int~n\c
	        unify_ast_~w(term_t t, const clingo_ast_~w_t *ast) {~n',
	       [Name, Name]),
	format('  switch( ast->type ) {~n', []),
	maplist(union_member(Name), Union),
	format('  }~n', []),
	format('}~n~n', []), !.
translate(Name=Struct) :-
	Struct =.. [_Type|Args],
	format('static int~n\c
		unify_ast_~w(term_t t, const clingo_ast_~w_t *ast) {~n',
	       [Name, Name]),
	length(Args, Arity),
	format('  term_t tmp = PL_new_term_ref();~n', []),
	format('  static functor_t f = 0;~n~n', []),
	format('  if ( !f )~n', []),
	format('    f = PL_new_functor(PL_new_atom("~w"), ~w);~n', [Name, Arity]),
	format('  if ( !PL_unify_functor(t, f) )~n', []),
	format('    return FALSE;~n~n'),
	forall(nth1(I, Args, Arg),
	       struct_member(I, Arg)),
	format('  return TRUE;~n', []),
	format('}~n~n', []), !.
translate(Struct) :-
	Struct \= (_=_),
	struct_member_name(Struct, CType), !,
	translate(CType=Struct).
translate(Term) :-
	format('TODO: ~p~n', [Term]).

union_member(Name, Struct) :-
	Struct =.. [Type|_Args],
	clingo_name(Type, CType),
	format('    case clingo_ast_~w_type_~w:~n', [Name, CType]),
	format('      return unify_ast_~w(t, ast->~w);~n', [CType, CType]).

struct_member(I, Arg) :-
	format('  if ( !PL_get_arg(~w, t, tmp) )~n', [I]),
	format('    return FALSE;~n', []),
	struct_member(Arg, tmp, 0), !.
struct_member(I, Arg) :-
	format('TODO: ~w, ~w~n', [I, Arg]).

struct_member(Name:str, Tmp, Indent) :-
	format('~t~*|  if ( !PL_unify_atom_chars(~w, ast->~w) )~n',
	       [Indent, Tmp, Name]),
	format('~t~*|    return FALSE;~n', [Indent]), !.
struct_member(Name:Type?, Tmp, Indent) :-
	struct_member_name(Type, CType),
	format('~t~*|  if ( (ast->~w ? !unify_ast_~w(~w, ast->~w) : \c
			               !unify_ast_null(~w)) )~n',
	       [Indent, Name, CType, Tmp, Name, Tmp]),
	format('~t~*|    return FALSE;~n', [Indent]), !.
struct_member(Name:Type*, Tmp, Indent) :-
	format('~t~*|  {~n', [Indent]),
	format('~t~*|    int i;~n', [Indent]),
	format('~t~*|    term_t head = PL_copy_term_ref(~w);~n', [Indent, Tmp]),
	format('~t~*|    term_t tail = PL_new_term_ref();~n~n', [Indent]),
	format('~t~*|    for(i=0; i<ast->~w_size; i++) {~n', [Indent, Name]),
	format('~t~*|      if ( !PL_unify_list(tail, head, tail) )~n', [Indent]),
	format('~t~*|        return FALSE;~n', [Indent]),
	format(atom(Element), '~w[i]', [Name]),
	Indent2 is Indent + 4,
	struct_member(Element:Type, head, Indent2),
	format('~t~*|    }~n', [Indent]),
	format('~t~*|    if ( !PL_unify_nil(tail) )~n', [Indent]),
	format('~t~*|      return FALSE;~n', [Indent]),
	format('~t~*|  }~n', [Indent]).
struct_member(Name:Type+, Tmp, Indent) :- !,
	struct_member(Name:Type*, Tmp, Indent).
struct_member(Name:Type, Tmp, Indent) :-
	struct_member_name(Type, CType),
	format('~t~*|  if ( !unify_ast_~w(~w, &ast->~w) )~n',
	       [Indent, CType, Tmp, Name]),
	format('~t~*|    return FALSE;~n', [Indent]), !.

struct_member_name(Type, CType) :-
	atom(Type), !,
	clingo_name(Type, CType).
struct_member_name(Compound, CType) :-
	functor(Compound, Type, _),
	clingo_name(Type, CType).

		 /*******************************
		 *	      UTIL		*
		 *******************************/

bar_list((H|T)) --> !,
	[H],
	bar_list(T).
bar_list(H) -->
	[H].

clingo_name(Capitalised, ClingoName) :-
	camel_snake(Capitalised, ClingoName).

camel_snake(Camel, Snake) :-
	atom_codes(Camel, [C0|Codes]),
	phrase(snake(SnakeCodes), Codes),
	code_type(C, to_lower(C0)),
	atom_codes(Snake, [C|SnakeCodes]).

snake([0'c,0's,0'p|T]) -->
	"CSP", !,
	snake(T).
snake([0'_,H|T]) -->
	[C],
	{ code_type(C, upper), !,
	  code_type(H, to_lower(C))
	},
	snake(T).
snake([H|T]) -->
	[H], !,
	snake(T).
snake([]) -->
	[].

