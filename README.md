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

