- Add info to the Prolog context to
  - Avoid illegal (threaded) usage		[OK]
  - Add clingo_close/1				[OK]
  - Leak due to call_function			[OK]
  - Keep track of loaded program and params

- Define Clingo Term Syntax
  - Problems
    - #keyword
      Define statement keywords as xfx operators.
    - Var``
      Not allowed in term-representation
    - !=
      Use \=

- Fix XREF and syntax highlighting
  - Register sdl start/end with xref:
    prolog:dsl((:- begin_clingo(_)), push_dsl(clingo)).
    prolog:dsl((:- end_clingo),      pop_dsl(clingo)).
    --> may push/pop syntax
    --> registers line/char range dealing with clingo
    --> calls pre-processing hook for lang
  - Also handled for full buffer colouring:
    --> prolog_load_context(dsl, DSL).
    --> xref_context(dsl, DSL).
  - Incremental colouring can ask for this too

# BUGS

## Syntax errors are printed:

?- const(M).
<block>:1:15-16: error: syntax error, unexpected <IDENTIFIER>

ERROR: Unknown error term: clingo_error('runtime error')
?-
