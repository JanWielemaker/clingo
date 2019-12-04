:- module(test_clingo,
      [ test_clingo/0
      ]).
:- use_module(library(plunit)).
:- use_module(library(clingo)).

test_clingo :-
    run_tests([ symbolic_atoms
              , assumptions
              , external
              , params
              , coloring
          ]).


:- begin_tests(symbolic_atoms).

test(fact, Info == symbol(a,fact)) :-
    ground(Clingo, "a."),
    clingo_symbol_lookup(Clingo, a, Info).
test(choice, Info == symbol(a,unknown)) :-
    ground(Clingo, "{a}."),
    clingo_symbol_lookup(Clingo, a, Info).
test(external, Info == symbol(a,external)) :-
    ground(Clingo, "#external a."),
    clingo_symbol_lookup(Clingo, a, Info).

:- end_tests(symbolic_atoms).


:- begin_tests(assumptions).

test(empty, Info == [[],[p(1)],[p(1),p(x)],[p(x)]]) :-
    ground(Clingo, "{p(1); p(x)}."),
    solve(Clingo, [ ~p(4) ], Info).

test(contradict, Info == []) :-
    ground(Clingo, "{p(1); p(x)}."),
    solve(Clingo, [ p(4) ], Info).

test(positive, Info == [[p(1)],[p(1),p(x)]]) :-
    ground(Clingo, "{p(1); p(x)}."),
    solve(Clingo, [ p(1) ], Info).

test(mixed, Info == [[p(1)]]) :-
    ground(Clingo, "{p(1); p(x)}."),
    solve(Clingo, [ p(1), ~p(x) ], Info).

:- end_tests(assumptions).


:- begin_tests(external).

test(true, Info == [[[p(a)]],[[]],[[],[p(a)]],[[]]]) :-
    ground(Clingo, "#external p(a)."),
    clingo_assign_external(Clingo, p(a), true),
    solve(Clingo, [], M1),
    clingo_assign_external(Clingo, p(a), false),
    solve(Clingo, [], M2),
    clingo_assign_external(Clingo, p(a), _),
    solve(Clingo, [], M3),
    clingo_release_external(Clingo, p(a)),
    solve(Clingo, [], M4),
    Info = [M1, M2, M3, M4].

:- end_tests(external).


:- begin_tests(params).

test(true, Info == [[p(4)]]) :-
    clingo_new(Clingo, []),
    clingo_add(Clingo, prog(x), "p(x)."),
    clingo_ground(Clingo, [ prog(4) ]),
    solve(Clingo, [], Info).

:- end_tests(params).


:- begin_tests(coloring).

% TODO: I do not know enough about prolog to figure out why predicates cannot
%       be called from unit tests.
/*
color(red).
color(blue).
color(green).

% from http://www.dai.ed.ac.uk/groups/ssp/bookpages/quickprolog/node34.html
ngb(portugal, [spain]).
ngb(spain, [portugal,france]).
ngb(france, [spain,belgium,switzerland,w_germany,italy]).
ngb(belgium, [france,w_germany,netherlands]).
ngb(netherlands, [belgium,w_germany]).
ngb(w_germany, [netherlands,belgium,france,switzerland,austria,denmark]).
ngb(switzerland, [france,w_germany,austria,italy]).
ngb(austria, [w_germany,switzerland,italy]).
ngb(italy, [france,switzerland,austria]).
ngb(denmark, [w_germany]).

country(C) :-
    ngb(C, _).

border(A,B) :-
    ngb(A, Borders),
    member(B, Borders).

test(color, Info == []) :-
    ground(Clingo,
           "1 {color(X,I) : c(I)} 1 :- v(X).
            :- color(X,I), color(Y,I), e(X,Y), c(I).
            c(C) :- color(C) = @color(1, user).
            v(X) :- country(X) = @country(1, user).
            e(X,Y) :- border(X,Y) = @border(2, user).
            #show color/2."),
    solve(Clingo, [], Info).
*/

:- end_tests(coloring).


ground(Clingo, Program) :-
    clingo_new(Clingo, []),
    clingo_add(Clingo, base, Program),
    clingo_ground(Clingo, [base]).

solve(Clingo, Assumptions, SortedModels) :-
    findall(M, clingo_solve(Clingo, Assumptions, M), Models),
    sort(Models, SortedModels).
