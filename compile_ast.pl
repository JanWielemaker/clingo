:- op(200, yf, ?).
:- op(200, yf, *).
:- op(200, yf, +).

read_ast(File, AST) :-
	setup_call_cleanup(
	    set_prolog_flag(allow_variable_name_as_functor, true),
	    setup_call_cleanup(
		open(File, read, In),
		read_ast_stream(In, AST),
		close(In)),
	    set_prolog_flag(allow_variable_name_as_functor, false)).

read_ast_stream(In, AST) :-
	at_end_of_stream(In), !,
	AST = [].
read_ast_stream(In, [H|T]) :-
	read_term(In, H, [variable_names(Bindings)]),
	maplist(call, Bindings),
	read_ast_stream(In, T).

