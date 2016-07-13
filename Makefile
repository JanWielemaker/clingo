################################################################
# Build the Clingo interface for SWI-Prolog
################################################################

-include FLAGS

CLINGOHOME?=${HOME}/clingo
BUILD?=debug

SWIPLLD?=swipl-ld
SWIPL?=swipl

COFLAGS?=-gdwarf-2 -g3 -O0

CLINGOLIBDIR=$(CLINGOHOME)/build/$(BUILD)
CFLAGS=-I$(CLINGOHOME)/libgringo -W -Wall $(COFLAGS)
LIBS=-L$(CLINGOLIBDIR) -lclingo
LDFLAGS=-cc-options,-std=c99 -Wl,-rpath=$(CLINGOLIBDIR) -shared
SO=so

all: clingo.$(SO)

FLAGS:
	rm -f FLAGS
	echo "CLINGOHOME:=$(CLINGOHOME)" >> FLAGS
	echo "BUILD:=$(BUILD)" >> FLAGS
	echo "SWIPLLD:=$(SWIPLLD)" >> FLAGS
	echo "SWIPL=$(SWIPL)" >> FLAGS
	echo "PLFLAGS=-pl \$$(SWIPL)" >> FLAGS
	echo "COFLAGS=$(COFLAGS)" >> FLAGS

SRC=clingo.c

clingo.$(SO): $(SRC) FLAGS
	$(SWIPLLD) $(PLFLAGS) $(CFLAGS) $(LDFLAGS) -o $@ $(SRC) $(LIBS)

install: clingo.$(SO)
	eval $$($(SWIPL) --dump-runtime-variables) && \
	install -m 644 clingo.pl $$PLBASE/library
	eval $$($(SWIPL) --dump-runtime-variables) && \
	install -m 644 clingo.$(SO) $$PLBASE/lib/$$PLARCH
	$(SWIPL) -g make,halt -t 'halt(1)'
