################################################################
# Build the Clingo interface for SWI-Prolog
################################################################

SWIPLLD=swipl-ld
SWIPL=swipl
PLFLAGS=-pl $(SWIPL)
CFLAGS=-W -Wall -gdwarf-2 -g3 -O0
LDFLAGS=-cc-options,-std=c99 -shared
LIBS=-lclingo
SO=so

all: clingo.$(SO)

SRC=clingo.c

.PHONY: check

check:
	swipl -p library=. -p foreign=. -f test_clingo.pl -g test_clingo,halt -t 'halt(1)' -q

.PHONY: clean

clean:
	rm -f clingo.$(SO)

clingo.$(SO): $(SRC)
	$(SWIPLLD) $(PLFLAGS) $(CFLAGS) $(LDFLAGS) -o $@ $(SRC) $(LIBS)

install: clingo.$(SO)
	eval $$($(SWIPL) --dump-runtime-variables) && \
	install -m 644 clingo.pl $$PLBASE/library
	eval $$($(SWIPL) --dump-runtime-variables) && \
	install -m 644 clingo.$(SO) $$PLBASE/lib/$$PLARCH
	$(SWIPL) -g make,halt -t 'halt(1)'
