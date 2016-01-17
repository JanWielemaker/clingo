################################################################
# Build the Clingo interface for SWI-Prolog
################################################################

# Edit
CLINGOHOME=/home/jan/3rdparty/gringo-claspoutput
BUILD=release

# Edit and uncomment if `swipl` is not in $PATH
SWIPLLD=swipl-ld
SWIPL=swipl
# PLFLAGS=-pl $(SWIPL)

COFLAGS=-O2
# Uncomment for debugging
# COFLAGS= -gdwarf-2 -g3

CLINGOLIBDIR=$(CLINGOHOME)/build/$(BUILD)
CFLAGS=-I$(CLINGOHOME)/libcclingo -Wall $(COFLAGS)
LIBS=-L$(CLINGOLIBDIR) -lcclingo
LDFLAGS=-cc-options,-std=c99 -Wl,-rpath=$(CLINGOLIBDIR) -shared
SO=so

SRC=clingo.c

all:	clingo.$(SO)

clingo.$(SO): $(SRC) Makefile
	$(SWIPLLD) $(PLFLAGS) $(CFLAGS) $(LDFLAGS) -o $@ $(SRC) $(LIBS)

install: clingo.$(SO)
	eval $$($(SWIPL) --dump-runtime-variables) && \
	install -m 644 clingo.pl $$PLBASE/library
	eval $$($(SWIPL) --dump-runtime-variables) && \
	install -m 644 clingo.$(SO) $$PLBASE/lib/$$PLARCH
	$(SWIPL) -g make,halt -t 'halt(1)'
