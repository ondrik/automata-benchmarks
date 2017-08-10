%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_state(S) :-
  known(KL),
  member(S,KL),
  !.

new_state(S) :-
  retract(known(KL)),
  retract(todo(TL)),
  assertz(known([S|KL])),
  assertz(todo([S|TL])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

first_step(From,TrL,[trans(From,A,Via)|ToDoL]) :-
  second_step(From,A,Via,TrL).

first_step(From,TrL,[_|ToDoL]) :- first_step(From,TrL,ToDoL).

second_step(From,A,Via,[trans(Via,B,To)|ToDoL]) :-
  write('trans('),
  write(From),
  write(','''),write(B),write('''/'''),write(A),write(''','),
  write(To),
  write('),'),
  nl,
  new_state(To),
  fail.

second_step(From,A,Via,[_|ToDoL]) :- second_step(From,A,Via,ToDoL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_coupl_tr(TrL) :-
  retract(todo([])),
  retract(known(_)),
  !.

do_coupl_tr(TrL) :-
  retract(todo([From|TL])),
  assertz(todo(TL)),
  (first_step(From,TrL,TrL);do_coupl_tr(TrL)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coupl_tr(From,TrL) :-
  assertz(known([From])),
  assertz(todo([From])),
  do_coupl_tr(TrL).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
