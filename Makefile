SRC=clingo.c
CLINGOLIBDIR=/home/jan/src/gringo-claspoutput/build/debug
CFLAGS=-I/home/jan/src/gringo-claspoutput/libcclingo
LIBS=-L$(CLINGOLIBDIR) -lcclingo

clingo.so: $(SRC)
	swipl-ld $(CFLAGS) -Wl,-rpath=$(CLINGOLIBDIR) -shared -o $@ $(SRC) $(LIBS)
