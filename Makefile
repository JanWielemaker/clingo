################################################################
# Build the Clingo interface for SWI-Prolog
################################################################

# Edit
CLINGOHOME=/home/jan/src/gringo-claspoutput
BUILD=debug

# Edit and uncomment if `swipl` is not in $PATH
SWIPLLD=swipl-ld
# PLFLAGS=-pl /usr/opt/swi-prolog/bin/swipl

COFLAGS=-O2
# Uncomment for debugging
# COFLAGS= -gdwarf-2 -g3

CLINGOLIBDIR=$(CLINGOHOME)/build/$(BUILD)
CFLAGS=-I$(CLINGOHOME)/libcclingo -Wall $(COFLAGS)
LIBS=-L$(CLINGOLIBDIR) -lcclingo
LDFLAGS=-cc-options,-std=c99 -Wl,-rpath=$(CLINGOLIBDIR) -shared


SRC=clingo.c

clingo.so: $(SRC) Makefile
	$(SWIPLLD) $(PLFLAGS) $(CFLAGS) $(LDFLAGS) -o $@ $(SRC) $(LIBS)
