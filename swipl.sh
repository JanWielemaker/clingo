# Use this to start SWI-Prolog, loading clingo.pl and clingo.so from
# the current directory.

swipl -p foreign=. -p library=. $@
