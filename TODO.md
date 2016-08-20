- Add info to the Prolog context to
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

- Syntax errors are printed:

          ?- const(M).
          <block>:1:15-16: error: syntax error, unexpected <IDENTIFIER>

          ERROR: Unknown error term: clingo_error('runtime error')
          ?-

  - Such mesages can be intercepted registering a logger (the
    [clingo\_logger\_t
    callback](https://potassco.github.io/clingo/group__BasicTypes.html#gaff11abc056335394295ce2ffdc88daac)).

- Thread-safety
  - Clingo's control objects are not thread-safe. If there are more than two
    concurrent calls involving the same control object, a logic error should be
    raised.

- Error checking
  - not all return values of clingo functions are checked
  - there are a lot of prolog functions whose return values are not checked
    (even though not all of them might need checking)

- the sign of functions is not handled in unify\_value
