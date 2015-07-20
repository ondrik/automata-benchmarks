do_go(0,A,_,A) :- !.

do_go(N,A,TR,C) :-
  fsa_regex_compile(minimize(range(compose(fa(A),fa(TR)))),B),
  NN is N-1,
  do_go(NN,B,TR,C),
  !.

go(N,A,TR,C) :-
  statistics(cputime,[T1,_]),
  do_go(N,A,TR,C),
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

