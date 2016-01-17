# Clingo ASP Solver binding for SWI-Prolog

This repository contains work in progress for embedding the Clingo ASP
solver in SWI-Prolog.  To use this, you need:

  - Clingo.  Currently you need to development version from the SVN
    repo, which may be downloaded using

    ```{shell}
    svn co http://svn.code.sf.net/p/potassco/code/branches/gringo-claspoutput
    ```

  - A recent copy of the _development_ version of SWI-Prolog (7.3.x)

Please consult the file `INSTALL` in   the downloaded clingo version and
install   the   target    **cexample**    as     shown    below.    Omit
`--build-dir=release` to compile the debug version.

```{shell}
scons --build-dir=release cexample
```

After both SWI-Prolog and Clingo are installed, edit `Makefile` to suit
your installation. See instructions in the file. After that running
`make` should suffice to build the interface. The interface may be
tested by running e.g.:

```{shell}
./swipl.sh examples/map_color.pl
?- map_color(Colors).
```

Finally, the system is installed as a SWI-Prolog library using `make
install`.


## Status

The basic communication is working. Please do **not expect the current
interface to be stable**. Currently, the interface allows for creating a
Clingo program from a string or loading one from a file or from a (not
yet settled) representation as a Prolog term. The program can be asked
for its stable models (if any) on backtracking. A model is represented
as a Prolog list of ground _atoms_ in the predicate logic sense.  The
interface suppors Clingo's abilities to deal with external data and a
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
