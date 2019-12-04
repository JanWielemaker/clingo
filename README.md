# Clingo ASP Solver binding for SWI-Prolog

This repository contains work in progress for embedding the Clingo ASP
solver in SWI-Prolog.  To use this, you need:

  - Clingo.  You need clingo version 5, which is available on github:
    - https://github.com/potassco/clingo
  - A recent copy of SWI-Prolog (>= 7.3.x):
    - https://www.swi-prolog.org/

After both SWI-Prolog and Clingo are installed, edit the `Makefile` to
suit your installation. See instructions in the file. After that running
`make` should suffice to build the interface. The interface may be
tested by running e.g.:

```{shell}
./swipl.sh examples/map_color.pl
?- map_color(Colors).
```

Finally, the system can be installed as a SWI-Prolog library using 
`make install`.


## Status

The basic communication is working. Please do **not expect the current
interface to be stable**. Currently, the interface allows for creating a
Clingo program from a string or loading one from a file or from a (not
yet settled) representation as a Prolog term. The program can be asked
for its stable models (if any) on backtracking. A model is represented
as a Prolog list of ground _atoms_ in the predicate logic sense.  The
interface supports Clingo's abilities to deal with external data and a
changing world:

  - Clingo atoms may refer to a Prolog predicate
  - Clingo's support for _parameterized_ grounding is supported
  - Clingo _externals_ are supported
  - Clingo _assumptions_ are supported


### Plans

  - Inspect and set configuration options
  - Clingo will provide access to the AST (Abstract Syntax Tree) which
    allow for:
    - Specifying Clingo source using Quasi Quotation fragments
    - Pass identifier, string and variable values that are valid
      Prolog atoms, strings or variables, but invalid in Clingo's
      syntax.
  - More high level interaction.


### Further reading:

  - The Clingo user guide can be found here
    - https://github.com/potassco/guide/releases
  - The Clingo C API is documented here
    - https://potassco.org/clingo/

