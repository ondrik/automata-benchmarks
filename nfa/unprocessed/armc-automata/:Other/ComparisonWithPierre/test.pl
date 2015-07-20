do_go(TR) :-
  retract(automaton(A)),
  retract(todo(N)),
  fsa_regex_compile(minimize(range(compose(fa(A),fa(TR)))),B),
  NN is N-1,
  assertz(automaton(B)),
  assertz(todo(NN)),
  !.

go(N,A,TR,C) :-
  statistics(cputime,[T1,_]),
  assertz(automaton(A)),
  assertz(todo(N)),
  repeat,
    do_go(TR),
    todo(0),
  retract(automaton(C)),
  retract(todo(_)),
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

