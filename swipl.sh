# Use this to start SWI-Prolog, loading clingo.pl and clingo.so from
# the current directory.

exec swipl -p foreign=. -p library=. $@
