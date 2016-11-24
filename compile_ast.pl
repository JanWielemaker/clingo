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
	{ Dotted =.. [.,Left,Right], !,
	  format(atom(Flat), '~w_~w', [Left, Right])
	},
	arg_type(Flat).
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
declare(symbolic_atom=_Struct) :-
	format('static int \c
		unify_ast_symbolic_atom(term_t t, const clingo_ast_term_t *ast);~n',
	       []), !.
declare(Name=_Struct) :-
	format('static int \c
		unify_ast_~w(term_t t, const clingo_ast_~w_t *ast);~n',
	       [Name, Name]), !.
declare(union_of(Name, Struct)) :-
	struct_member_name(Struct, CType),
	format('static int \c
		unify_ast_~w_u_~w(term_t t, const clingo_ast_~w_t *ast);~n',
	       [CType, Name, Name]), !.
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
translate(symbolic_atom=_Struct) :-
	format('static int~n\c
		unify_ast_symbolic_atom(term_t t, const clingo_ast_term_t *ast) {~n',
	       []),
	format('  term_t tmp = PL_new_term_ref();~n', []),
	format('  static functor_t f = 0;~n~n', []),
	format('  if ( !f )~n', []),
	format('    f = PL_new_functor(PL_new_atom("symbolic_atom"), 1);~n',[]),
	format('  if ( !PL_unify_functor(t, f) )~n', []),
	format('    return FALSE;~n~n'),
	format('  if ( !PL_get_arg(1, t, tmp) )~n', []),
	format('    return FALSE;~n', []),
	format('  if ( !unify_ast_term(tmp, ast) )~n', []),
	format('    return FALSE;~n', []),
	format('  return TRUE;~n', []),
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
translate(union_of(Name, Struct)) :-
	Struct =.. [_Type|Args],
	struct_member_name(Struct, CType),
	format('static int~n\c
		unify_ast_~w_u_~w(term_t t, const clingo_ast_~w_t *ast) {~n',
	       [CType, Name, Name]),
	length(Args, Arity),
	format('  term_t tmp = PL_new_term_ref();~n', []),
	format('  static functor_t f = 0;~n~n', []),
	format('  if ( !f )~n', []),
	format('    f = PL_new_functor(PL_new_atom("~w"), ~w);~n', [CType, Arity]),
	format('  if ( !PL_unify_functor(t, f) )~n', []),
	format('    return FALSE;~n~n'),
	forall(nth1(I, Args, Arg),
	       union_struct_member(CType, I, Arg)),
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
	enum_name(CType, ECType),
	format('    case clingo_ast_~w_type_~w:~n', [Name, ECType]),
	format('      return unify_ast_~w_u_~w(t, ast);~n',
	       [CType, Name]).

enum_name(theory_function, function) :- !.
enum_name(theory_unparsed_term, unparsed_term) :- !.
enum_name(symbolic_atom, symbolic) :- !.
enum_name(csp_literal, csp) :- !.
enum_name(Name, Name).


%%	union_struct_member(UnionName, I, MemberName:Arg)

union_struct_member(UnionName, I, MemberName:Arg) :-
	debug(ast, '~p', [union_struct_member(UnionName, I, MemberName:Arg)]),
	fail.
union_struct_member(_, I, sign:Arg) :- !,
	struct_member(I, sign:Arg).
union_struct_member(_, I, location:Arg) :- !,
	struct_member(I, location:Arg).
union_struct_member(_, I, symbol:Arg) :- !,
	struct_member(I, symbol:Arg).
union_struct_member(variable, I, name:Arg) :- !,
	struct_member(I, variable:Arg).
union_struct_member(theory_function, I, Arg) :- !,
	union_struct_member(function, I, Arg).
union_struct_member(theory_unparsed_term, I, Arg) :- !,
	union_struct_member(unparsed_term, I, Arg).
union_struct_member(Name, I, U:Arg) :-
	format(atom(UName), '~w->~w', [Name, U]),
	struct_member(I, UName:Arg).

struct_member(I, Arg) :-
	format('  if ( !PL_get_arg(~w, t, tmp) )~n', [I]),
	format('    return FALSE;~n', []),
	struct_member(Arg, tmp, 0), !.
struct_member(I, Arg) :-
	format('TODO: ~w, ~w~n', [I, Arg]).

struct_member(Arg, _Tmp, _Indent) :-
	debug(ast(struct_member), '~p', [struct_member(Arg)]),
	fail.
struct_member(Name:str, Tmp, Indent) :-
	format('~t~*|  if ( !PL_unify_atom_chars(~w, ast->~w) )~n',
	       [Indent, Tmp, Name]),
	format('~t~*|    return FALSE;~n', [Indent]), !.
struct_member(Name:bool, Tmp, Indent) :-
	format('~t~*|  if ( !PL_unify_bool(~w, ast->~w) )~n',
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
	size_name(Name, SizeName),
	format('~t~*|    for(i=0; i<ast->~w; i++) {~n', [Indent, SizeName]),
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

size_name('function->arguments', 'function->size') :- !.
size_name('external_function->arguments', 'external_function->size') :- !.
size_name('pool->arguments', 'pool->size') :- !.
size_name('tuple->terms', 'tuple->size') :- !.
size_name('list->terms', 'list->size') :- !.
size_name('set->terms', 'set->size') :- !.
size_name('terms', size) :- !.
size_name('operators', size) :- !.
%size_name('condition', size) :- !.
size_name('unparsed_term->elements', 'unparsed_term->size') :- !.
size_name('body_aggregate->elements', 'body_aggregate->size') :- !.
size_name('head_aggregate->elements', 'head_aggregate->size') :- !.
size_name('disjunction->elements', 'disjunction->size') :- !.
size_name('rule->body', 'rule->size') :- !.
size_name('program->parameters', 'program->size') :- !.
size_name(Name, SizeName) :-
	debug(ast(size), '~p ~p', [Name, SizeName]),
	atom_concat(Name, '_size', SizeName).


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

snake([0's,0'p|T]) -->
	"SP", !,
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

