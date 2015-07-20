%%%%%%%%%%%%%%%%%%%%%%%%

%% :- consult('/usr/local/src/FSA/src-compiled-swi/fsa_library').

%% :- consult('/usr/local/src/FSA/src-compiled-yap/fsa_library').
:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_intr(A1,A2) :- fsa_regex_compile(intersect(fa(A1),fa(A2)),I), empty(I).

%-------------------------------------------------------------------------------

add_to_set(Elem,SetL,SetL) :- member(Elem,SetL), !.

add_to_set(Elem,SetL,[Elem|SetL]).

%...............................................................................

%%% Removes just the first occurrence of Elem (if any)

rem_from_set(Elem,[Elem|SetL],SetL) :- !.

rem_from_set(Elem1,[Elem2|SetL],[Elem2|NewSetL]) :- rem_from_set(Elem1,SetL,NewSetL).

%-------------------------------------------------------------------------------

in_set_of(Elem,Key,TbL) :- member([Key,SetL],TbL),member(Elem,SetL).

%...............................................................................

rem_set_of(Key,[[Key,SetL]|TbL],SetL,TbL) :- !.

rem_set_of(Key,[X|TbL],SetL,[X|NewTbL]) :- rem_set_of(Key,TbL,SetL,NewTbL).

%...............................................................................

add_to_set_of(Elem,Key,TbL,[[Key,NewSetL]|NewTbLa]) :-
  ( (rem_set_of(Key,TbL,SetL,NewTbLa), !, add_to_set(Elem,SetL,NewSetL));
    (NewSetL=[Elem],NewTbLa=TbL) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing inclusion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_included(A,B) :-
  empty(E),
  fsa_regex_compile(mb(intersect(fa(A),complement(fa(B)))),E),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. forward languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Labelling states of a "concrete" automaton by states of a "predicate" automaton
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_for_conc([[QConc,QPred]|BSmtReachL],ToDoTbL,NewToDoTbL,ConcTbL,NewConcTbL) :-
  ( (in_set_of(QPred,QConc,ConcTbL), !,
     new_for_conc(BSmtReachL,ToDoTbL,NewToDoTbL,ConcTbL,NewConcTbL));
    (add_to_set_of(QPred,QConc,ConcTbL,NewConcTbLa),
     add_to_set_of(QPred,QConc,ToDoTbL,NewToDoTbLa),
     new_for_conc(BSmtReachL,NewToDoTbLa,NewToDoTbL,NewConcTbLa,NewConcTbL)) ), !.

new_for_conc([],ToDoTbL,ToDoTbL,ConcTbL,ConcTbL).

%-------------------------------------------------------------------------------

%%% Removes the first compatible transition (with a nonempty intersection of the labels)

%%% We assume that not_in cannot appear - it can appear only when dealing with automata
%%% obtained by negation, which should not be the case in the collapsing method.
%%% If not_in might appear, a further intersection of I with an explicitly given
%%% alphabet is necessary!!!

rem_compatible_from_set([trans(QPred1,B,QPred2)|NewTrPredL],A,QPred2,QPred1,NewTrPredL) :-

  fsa_preds:conjunction(A,B,I),
  ( (I\=not_in([_|_])); (write('Error: not_in appeared in rem_compatible_from_set!'),nl,halt) ),



  !.

rem_compatible_from_set([_|TrPredL],A,QPred2,QPred1,NewTrPredL) :-
  rem_compatible_from_set(TrPredL,A,QPred2,QPred1,NewTrPredL).

%-------------------------------------------------------------------------------

do_step_back(trans(QConc1,A,QConc2),TrPredL,QPred2,BSmtReachL) :-
  rem_compatible_from_set(TrPredL,A,QPred2,QPred1,NewTrPredL),
  !,
  do_step_back(trans(QConc1,A,QConc2),NewTrPredL,QPred2,BSmtReachLa),
  BSmtReachL=[[QConc1,QPred1]|BSmtReachLa],
  !.

do_step_back(_,_,_,[]).

%...............................................................................

step_back([trans(QConc1,A,QConc2)|TrConcL],TrPredL,QConc2,QPred2,BSmtReachL) :-
  !,
  do_step_back(trans(QConc1,A,QConc2),TrPredL,QPred2,BSmtReachLa),
  step_back(TrConcL,TrPredL,QConc2,QPred2,BSmtReachLb),
  append(BSmtReachLa,BSmtReachLb,BSmtReachL),
  !.

step_back([_|TrConcL],TrPredL,QConc2,QPred2,BSmtReachL) :-
  !,
  step_back(TrConcL,TrPredL,QConc2,QPred2,BSmtReachL),
  !.

step_back([],_,_,_,[]).

%-------------------------------------------------------------------------------

%%% In case of troubles with the size of Prolog stacks, this can be optimized:

repeat_step_back([[QConc,[QPred|QPredL]]|ToDoTbL],ConcTbL,NewConcTbL,ConcTrL,PredTrL) :-
  !,
  step_back(ConcTrL,PredTrL,QConc,QPred,BSmtReachL),
  NewToDoTbLa=[[QConc,QPredL]|ToDoTbL],
  new_for_conc(BSmtReachL,NewToDoTbLa,NewToDoTbLb,ConcTbL,NewConcTbLb),
  repeat_step_back(NewToDoTbLb,NewConcTbLb,NewConcTbL,ConcTrL,PredTrL),
  !.

repeat_step_back([[_,[]]|ToDoTbL],ConcTbL,NewConcTbL,ConcTrL,PredTrL) :-
  !,
  repeat_step_back(ToDoTbL,ConcTbL,NewConcTbL,ConcTrL,PredTrL),
  !.

repeat_step_back([],ConcTbL,ConcTbL,_,_).

%-------------------------------------------------------------------------------

%%% We label each given (final/initial) concrete state by all given (final/initial) predicate states AND
%%% all other states by the empty set...

initial_labels(N,FConcL,FPredL,[[N,FPredL]|LabTabL]) :-
  member(N,FConcL),
  !,
  NN is N-1,
  initial_labels(NN,FConcL,FPredL,LabTabL), !.

initial_labels(N,FConcL,FPredL,[[N,[]]|LabTabL]) :-
  N >= 0,
  !,
  NN is N-1,
  initial_labels(NN,FConcL,FPredL,LabTabL), !.

initial_labels(_,_,_,[]).

%-------------------------------------------------------------------------------

add_labels([[Q,LabL]|LabTabL],ImpStL) :-
  %% sort(LabL,UseLabL), %%% Choose this or vvv
  ( (ImpStL=all, !, sort(LabL,UseLabL)); filter_and_sort(LabL,ImpStL,UseLabL)), %%% Choose this or ^^^
  ( (retract(label(Q,QLabLL)), !, assertz(label(Q,[UseLabL|QLabLL])));
    (assertz(label(Q,[UseLabL]))) ),
  add_labels(LabTabL,ImpStL),
  !.

add_labels([],_).

%-------------------------------------------------------------------------------

%%% Choose one of the following:

%%% Preserve just labelling by the initial states (sorting does not make sens here).
%%% We suppose just one initial state identified as 0 here...

%% filter_and_sort(LabL,_,UseLabL) :-
%% ( (member(0,LabL), !, UseLabL=[0]); UseLabL=[] ), !.

%%% Preserve just labelling by the given important states and then sort it.

filter_and_sort(LabL,ImpStL,FiltSortLabL) :-
  sort(LabL,SortLabL),
  intersect_ord(SortLabL,ImpStL,FiltSortLabL),
  !.

%-------------------------------------------------------------------------------

%%% We suppose both lists to be sorted and have unique elements.

intersect_ord([X|L1],[X|L2],[X|L3]) :- !, intersect_ord(L1,L2,L3), !.

intersect_ord([X|L1],[Y|L2],L3) :- X < Y, !, intersect_ord(L1,[Y|L2],L3), !.

intersect_ord([X|L1],[Y|L2],L3) :- X > Y, !, intersect_ord([X|L1],L2,L3), !.

intersect_ord(_,[],[]) :- !.

intersect_ord([],_,[]).

%-------------------------------------------------------------------------------

%%% Computing labelling wrt. forward languages by the important states of one of the predicates being used.

%%% For other predicate modules than r(fsa_preds)/r(fsa_frozen), a modification of rem_compatible_from_set
%%% is necessary...

compute_labels(fa(PredModule,ConcStN,_,ConcFinL,ConcTrL,_),fa(PredModule,_,_,PredFinL,PredTrL,_),ImpStL) :-

  ( (PredModule\=r(fsa_preds), !, write('Predicate module fsa_preds required!'),nl,halt); true ),



  ConcStNN is ConcStN-1,
  initial_labels(ConcStNN,ConcFinL,PredFinL,LabTabLa),
  repeat_step_back(LabTabLa,LabTabLa,LabTabL,ConcTrL,PredTrL),
  %% write('A (not filtered) set of forward labels: '),write(LabTabL),nl,
  add_labels(LabTabL,ImpStL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Suffix states of a predicate automaton to be taken into account when collapsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

common_suffix(fa(PredModule,ConcStN,_,ConcFinL,ConcTrL,_),fa(PredModule,_,_,PredFinL,PredTrL,_),ImpStL) :-
  ConcStNN is ConcStN-1,
  initial_labels(ConcStNN,ConcFinL,PredFinL,LabTabLa),
  repeat_step_back(LabTabLa,LabTabLa,LabTabL,ConcTrL,PredTrL),
  important_pred_suffix(LabTabL,[],ImpStL).

%%-------------------------------------------------------------------------------

important_pred_suffix([[_,L]|LabTabL],KnownL1,KnownL3) :-
  comp_imp_pred_suffix(L,KnownL1,KnownL2),
  important_pred_suffix(LabTabL,KnownL2,KnownL3),
  !.

important_pred_suffix([],KnownL,KnownL).

%%-------------------------------------------------------------------------------

comp_imp_pred_suffix([Q|L],KnownL1,KnownL3) :-
  insert_to_ord(Q,KnownL1,KnownL2),
  comp_imp_pred_suffix(L,KnownL2,KnownL3),
  !.

comp_imp_pred_suffix([],KnownL,KnownL).

%-------------------------------------------------------------------------------

insert_to_ord(Q1,[Q2|L],ResL) :-
  ( (Q1>Q2, !, insert_to_ord(Q1,L,ResLa),ResL=[Q2|ResLa]);
    (Q1<Q2, !, ResL=[Q1,Q2|L]);
    (ResL=[Q2|L]) ),
  !.

insert_to_ord(Q1,[],[Q1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reducing a predicate to the important suffix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Note: RedPred is not in the canonical FSA representation!

reduce_suffix(Pred,ImpStL,RedPred) :-
  Pred=fa(PrMod,NQ,_,FinL,TrL,[]),
  filter_trans(ImpStL,TrL,NewTrL),
  length(TrL,TN1),length(NewTrL,TN2),write('A transition reduction: '),write(TN1),write('->'),write(TN2),nl,
  %% fsa_construct_rename_states(PrMod,ImpStL,FinL,NewTrL,[],RedPred). %% Choose this or vvv
  RedPred=fa(PrMod,NQ,ImpStL,FinL,NewTrL,[]). %% Choose this or ^^^

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Try to find 1-2 key state(s) of a predicate automaton to be considered when collapsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

key_states(QAllCandidL,Conc,Pred,PredLangL,QKeyL) :-
  write('... Trying to find a single key state of the new predicate...'),nl,
  comp_key_states(QAllCandidL,QAllCandidL,Conc,Pred,PredLangL,QKeyL).

%-------------------------------------------------------------------------------

%% comp_key_states(_,QAllCandidL,_,_,_,QAllCandidL) :-
%% length(QAllCandidL,NumCand),
%% NumCand>120, !,
%% write('Too many candidates for finding key states - we take all candidates...'),nl,
%% !.

comp_key_states([Q|QCandidL],QAllCandidL,Conc,Pred,PredLangL,QKeyL) :-
  assertz(pred_lang([[Pred,[Q]]|PredLangL])),
  abstract(Conc,Abs),
  retract(pred_lang(_)),
  fsa_regex_compile(intersect(fa(Abs),fa(Pred)),Intrs),
  ( (empty(Intrs), !,
     write('... A key state of the new predicate is '),write(Q),write('.'),nl,
     QKeyL=[Q]);
    (comp_key_states(QCandidL,QAllCandidL,Conc,Pred,PredLangL,QKeyL)) ),
  !.

%%% Choose whether to try to find just a single kay state or also a key pair of states:

%% comp_key_states(_,QAllCandidL,_,_,_,QAllCandidL) :-
%% write('!!! There is NO single key state of the new predicate - we take all candidates...'),nl.

%% comp_key_states([],QAllCandidL,_,_,_,QAllCandidL) :-
%% length(QAllCandidL,NumCand),
%% NumCand>60, !,
%% write('Too many candidates for coupling - we take all candidates...'),nl,
%% !.

comp_key_states([],QAllCandidL,Conc,Pred,PredLangL,QKeyL) :-
  write('... Trying to find a key pair of states of the new predicate...'),nl,
  QAllCandidL=[_|QCandidL2],
  comp_two_key_states(QAllCandidL,QCandidL2,QAllCandidL,Conc,Pred,PredLangL,QKeyL).

%-------------------------------------------------------------------------------

comp_two_key_states([Q1|QCandidL1],[Q2|QCandidL2],QAllCandidL,Conc,Pred,PredLangL,QKeyL) :-
  !,
  assertz(pred_lang([[Pred,[Q1,Q2]]|PredLangL])),
  abstract(Conc,Abs),
  retract(pred_lang(_)),
  fsa_regex_compile(intersect(fa(Abs),fa(Pred)),Intrs),
  ( (empty(Intrs), !,
     write('... A key pair of states of the new predicate is '),write(Q1),write(', '),write(Q2),write('.'),nl,
     QKeyL=[Q1,Q2]);
    (comp_two_key_states([Q1|QCandidL1],QCandidL2,QAllCandidL,Conc,Pred,PredLangL,QKeyL)) ),
  !.

comp_two_key_states([_|QCandidL1],[],QAllCandidL,Conc,Pred,PredLangL,QKeyL) :-
  QCandidL1=[_|QCandidL2],
  !,
  comp_two_key_states(QCandidL1,QCandidL2,QAllCandidL,Conc,Pred,PredLangL,QKeyL),
  !.

comp_two_key_states(_,_,QAllCandidL,_,_,_,QAllCandidL) :-
  write('!!! There is NO key pair of states of the new predicate - we take all candidates...'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optimizing a predicate automaton with only a few key states
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_pred(Pred,[Q],Pred,[Q]) :-
  Pred=fa(_,_,[Q],_,_,_),
  !.

%%% Note: RedPred is not in the canonical FSA representation!

reduce_pred(Pred,QKeyL,RedPred,RenQKeyL) :-
  Pred=fa(PrMod,NQ,_,FinL,TrL,_),
  reachable_from_states(QKeyL,TrL,QKeyL,ReachL),
  filter_st(QKeyL,FinL,KeyFinL),
  append(ReachL,KeyFinL,QPreserveL),
  filter_trans(QPreserveL,TrL,NewTrL),
  length(TrL,TN1),length(NewTrL,TN2),write('A transition reduction: '),write(TN1),write('->'),write(TN2),nl,
  filter_st(QPreserveL,FinL,NewFinL),
  length(FinL,FN1),length(NewFinL,FN2),write('A final state reduction: '),write(FN1),write('->'),write(FN2),nl,
  %% to_jump_list(QKeyL,JL1), %% Choose this or vvv
  %% fsa_construct_rename_states(PrMod,[],NewFinL,NewTrL,JL1,RenPred),
  %% RenPred=fa(PrMod,NRenQ,[],RenFinL,RenTrL,JL2),
  %% to_jump_list(RenQKeyL,JL2),
  %% RedPred=fa(PrMod,NRenQ,RenQKeyL,RenFinL,RenTrL,[]).
  %% index_preserved_states(0,QPreserveL,NewNamesL), %% Choose this or ...
  %% length(QPreserveL,NRenQ),
  %% rename_preserved_states(NewFinL,NewNamesL,RenFinL),
  %% rename_preserved_transitions(NewTrL,NewNamesL,RenTrL),
  %% rename_preserved_states(QKeyL,NewNamesL,RenQKeyL),
  %% RedPred=fa(PrMod,NRenQ,RenQKeyL,RenFinL,RenTrL,[]).
  RedPred=fa(PrMod,NQ,QKeyL,NewFinL,NewTrL,[]),RenQKeyL=QKeyL. %% Choose this or ^^^

%%% ??? What is surprising is that the renaming and minimization of the number of states does not seem to bring anything...

%...............................................................................

to_jump_list([Q|L],[jump(Q,Q)|LL]) :- to_jump_list(L,LL).

to_jump_list([],[]).

%-------------------------------------------------------------------------------

%%% KnownL may be optimized via using an ordered set...

reachable_from_states([Q|ToDoL],TrL,KnownL,NewKnownL) :-
  reachable_from_state(Q,TrL,TrL,KnownL,NewKnownLa),
  reachable_from_states(ToDoL,TrL,NewKnownLa,NewKnownL),
  !.

reachable_from_states([],_,KnownL,KnownL).

%...............................................................................

reachable_from_state(Q1,[trans(Q1,_,Q2)|ToDoL],TrL,KnownL,NewKnownL) :-
  !,
  ( (member(Q2,KnownL), !, reachable_from_state(Q1,ToDoL,TrL,KnownL,NewKnownL));
    (reachable_from_state(Q2,TrL,TrL,[Q2|KnownL],NewKnownLa),
     reachable_from_state(Q1,ToDoL,TrL,NewKnownLa,NewKnownL)) ),
   !.

reachable_from_state(Q1,[_|ToDoL],TrL,KnownL,NewKnownL) :-
   reachable_from_state(Q1,ToDoL,TrL,KnownL,NewKnownL),
   !.

reachable_from_state(_,[],_,KnownL,KnownL).

%-------------------------------------------------------------------------------

filter_st([Q|QImpL],StL,[Q|NewStL]) :-
  member(Q,StL),
  !,
  filter_st(QImpL,StL,NewStL),
  !.

filter_st([_|QImpL],StL,NewStL) :-
  filter_st(QImpL,StL,NewStL),
  !.

filter_st([],_,[]).

%-------------------------------------------------------------------------------

filter_trans(QImpL,[trans(Q1,A,Q2)|TrL],[trans(Q1,A,Q2)|NewTrL]) :-
  member(Q1,QImpL),
  member(Q2,QImpL),
  !,
  filter_trans(QImpL,TrL,NewTrL),
  !.

filter_trans(QImpL,[_|TrL],NewTrL) :-
  filter_trans(QImpL,TrL,NewTrL),
  !.

filter_trans(_,[],[]).

%-------------------------------------------------------------------------------

index_preserved_states(N,[Q|L],[[Q,N]|LL]) :-
  NN is N+1,
  index_preserved_states(NN,L,LL),
  !.

index_preserved_states(_,[],[]).

%-------------------------------------------------------------------------------

rename_preserved_states([Q|L],NewNamesL,[QQ|LL]) :-
  member([Q,QQ],NewNamesL),
  rename_preserved_states(L,NewNamesL,LL),
  !.

rename_preserved_states([],_,[]).

%-------------------------------------------------------------------------------

rename_preserved_transitions([trans(Q1,A,Q2)|L],NewNamesL,[trans(QQ1,A,QQ2)|LL]) :-
  member([Q1,QQ1],NewNamesL),
  member([Q2,QQ2],NewNamesL),
  rename_preserved_transitions(L,NewNamesL,LL),
  !.

rename_preserved_transitions([],_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Collapsing an automaton
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_class(LabLL,[Q|QLa]) :-
  retract(label(Q,LabLL)),
  !,
  collect_class(LabLL,QLa),
  !.

collect_class(_,[]) :- !.

%-------------------------------------------------------------------------------

collect_partition([[Q|OthersL]|PartitionL]) :-
  retract(label(Q,LabLL)),
  !,
  collect_class(LabLL,OthersL),
  collect_partition(PartitionL),
  !.

collect_partition([]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_class([ClL|_],Q,ClL) :- member(Q,ClL), !.

find_class([_|PartitionL],Q,ClL) :- find_class(PartitionL,Q,ClL).

%-------------------------------------------------------------------------------

adjust_trans(PartitionL,[trans(Q1,A,Q2)|TrL],[trans(Cl1L,A,Cl2L)|NewTrL]) :-
  find_class(PartitionL,Q1,Cl1L),
  find_class(PartitionL,Q2,Cl2L),
  adjust_trans(PartitionL,TrL,NewTrL),
  !.

adjust_trans(_,[],[]).

%...............................................................................

adjust_st(PartitionL,[Q|QL],[ClL|NewQL]) :-
  find_class(PartitionL,Q,ClL),
  adjust_st(PartitionL,QL,NewQL),
  !.

adjust_st(_,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstracting automata by collapsing their states wrt. forward languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% If always all predicate states are considered in labelling, the first definition
%%% may be commented out.

do_abstr_fw(Conc,[[Pred,ImpStL]|PredL]) :-
  !,
  compute_labels(Conc,Pred,ImpStL),
  do_abstr_fw(Conc,PredL),
  !.

do_abstr_fw(Conc,[Pred|PredL]) :-
  compute_labels(Conc,Pred,all),
  do_abstr_fw(Conc,PredL),
  !.

do_abstr_fw(_,[]) :- !.

%-------------------------------------------------------------------------------



abstr_fw(Conc,Abstr) :-
  pred_lang(PredL), %% Choose this or vvv
  %% pred_lang(PredLa), ( (PredLa=[], !, empty(E),PredL=[E]); PredL=PredLa ), %% Choose this or ^^^
  %% write('Labelling wrt. the particular predicate automata...'),nl,
  do_abstr_fw(Conc,PredL),
  %% write('Computing the partition...'),nl,
  collect_partition(PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  Conc=fa(ConcSym,_,ConcIniL,ConcFinL,ConcTrL,_),
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstracting automata by collapsing their states - a DEBUGGING version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Requires new_pred to be initially asserted and its use enabled in the sources.

abstr_fw_debug(State,AbsState) :-
  abstr_fw(State,AbsState), check_coll(State,AbsState).

%...............................................................................

check_coll(State,AbsState) :-
  new_pred(PredL),
  do_check_coll(PredL,State,AbsState),
  %% write('vvv Check of collapsing passed...'),nl,
  !.

%...............................................................................

do_check_coll([Pred|PredL],State,AbsState) :-
  fsa_regex_compile(intersect(fa(Pred),fa(AbsState)),Intrs),
  ( (empty(Intrs), !, do_check_coll(PredL,State,AbsState));
    (write('xxx Check of collapsing failed...'),nl,
     fsa_write_file(xxxAbs,AbsState),
     fsa_write_file(xxxConc,State),
     fsa_write_file(xxxPred,Pred),
     fsa_regex_compile(intersect(fa(Pred),fa(State)),Intrs2),
     ( (empty(Intrs2), !, write('Disjunct with the concrete state...'),nl);
       (write('NOT disjunct with the concrete state either...'),nl) ),
     trace) ), !.

do_check_coll([],_,_) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. forward and backward languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Forward labelling of a "concrete" automaton by states of a "predicate" automaton
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_labels([[Q,LabL]|LabTabL],[[Q,SortLabL]|SortLabTabL]) :-
  sort(LabL,SortLabL),
  sort_labels(LabTabL,SortLabTabL),
  !.

sort_labels([],[]).

%-------------------------------------------------------------------------------

%%% Computing labelling wrt. forward languages by all states of a single predicate being used.

%%% For other predicate modules than r(fsa_preds), a modification of rem_compatible_from_set
%%% is necessary...

compute_uni_forw_labels(fa(PredModule,ConcStN,_,ConcFinL,ConcTrL,_),fa(PredModule,_,_,PredFinL,PredTrL,_),LabTabL) :-

  ( (PredModule\=r(fsa_preds), !, write('Predicate module fsa_preds required!'),nl,halt); true ),



  ConcStNN is ConcStN-1,
  initial_labels(ConcStNN,ConcFinL,PredFinL,LabTabLa),
  repeat_step_back(LabTabLa,LabTabLa,LabTabLb,ConcTrL,PredTrL),
  %% write('A (not filtered) set of forward labels: '),write(LabTabLb),nl,
  sort_labels(LabTabLb,LabTabL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Backward labelling of a "concrete" automaton by states of a "predicate" automaton
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_step_on(trans(QConc1,A,QConc2),TrPredL,QPred1,BSmtReachL) :-
  rem_compatible_from_set(TrPredL,A,QPred2,QPred1,NewTrPredL),
  !,
  do_step_on(trans(QConc1,A,QConc2),NewTrPredL,QPred1,BSmtReachLa),
  BSmtReachL=[[QConc2,QPred2]|BSmtReachLa],
  !.

do_step_on(_,_,_,[]).

%...............................................................................

step_on([trans(QConc1,A,QConc2)|TrConcL],TrPredL,QConc1,QPred1,BSmtReachL) :-
  !,
  do_step_on(trans(QConc1,A,QConc2),TrPredL,QPred1,BSmtReachLa),
  step_on(TrConcL,TrPredL,QConc1,QPred1,BSmtReachLb),
  append(BSmtReachLa,BSmtReachLb,BSmtReachL),
  !.

step_on([_|TrConcL],TrPredL,QConc1,QPred1,BSmtReachL) :-
  !,
  step_on(TrConcL,TrPredL,QConc1,QPred1,BSmtReachL),
  !.

step_on([],_,_,_,[]).

%-------------------------------------------------------------------------------

%%% In case of troubles with the size of Prolog stacks, this can be optimized:

repeat_step_on([[QConc,[QPred|QPredL]]|ToDoTbL],ConcTbL,NewConcTbL,ConcTrL,PredTrL) :-
  !,
  step_on(ConcTrL,PredTrL,QConc,QPred,BSmtReachL),
  NewToDoTbLa=[[QConc,QPredL]|ToDoTbL],
  new_for_conc(BSmtReachL,NewToDoTbLa,NewToDoTbLb,ConcTbL,NewConcTbLb),
  repeat_step_on(NewToDoTbLb,NewConcTbLb,NewConcTbL,ConcTrL,PredTrL),
  !.

repeat_step_on([[_,[]]|ToDoTbL],ConcTbL,NewConcTbL,ConcTrL,PredTrL) :-
  !,
  repeat_step_on(ToDoTbL,ConcTbL,NewConcTbL,ConcTrL,PredTrL),
  !.

repeat_step_on([],ConcTbL,ConcTbL,_,_).

%-------------------------------------------------------------------------------

%%% Computing labelling wrt. backward languages by all states of a single predicate being used.

%%% For other predicate modules than r(fsa_preds), a modification of rem_compatible_from_set
%%% is necessary...

compute_uni_bckw_labels(fa(PredModule,ConcStN,ConcIniL,_,ConcTrL,_),fa(PredModule,_,PredIniL,_,PredTrL,_),LabTabL) :-

  ( (PredModule\=r(fsa_preds), !, write('Predicate module fsa_preds required!'),nl,halt); true ),



  ConcStNN is ConcStN-1,
  initial_labels(ConcStNN,ConcIniL,PredIniL,LabTabLa),
  repeat_step_on(LabTabLa,LabTabLa,LabTabLb,ConcTrL,PredTrL),
  %% write('A (not filtered) set of backward labels: '),write(LabTabLb),nl,
  sort_labels(LabTabLb,LabTabL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Partitioning states wrt. forward and backward labels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list_of_n_states(M,N,[M|L]) :-
  M < N,
  !,
  MM is M+1,
  gen_list_of_n_states(MM,N,L),
  !.

gen_list_of_n_states(_,_,[]).

%-------------------------------------------------------------------------------

fwbw_compatible([Q1|L],FwLab2,BwLab2,FwLabTabL,BwLabTabL) :-
  !,
  member([Q1,FwLab1],FwLabTabL),
  intersect_ord(FwLab1,BwLab2,[]),
  member([Q1,BwLab1],BwLabTabL),
  intersect_ord(BwLab1,FwLab2,[]),
  fwbw_compatible(L,FwLab2,BwLab2,FwLabTabL,BwLabTabL),
  !.

fwbw_compatible([],_,_,_,_).

%-------------------------------------------------------------------------------

build_fwbw_class([Q2|ToTryL],ClassL,UnusedL,FwLabTabL,BwLabTabL,ComplClassL,AllUnusedL) :-
  member([Q2,FwLab2],FwLabTabL),
  member([Q2,BwLab2],BwLabTabL),
  fwbw_compatible(ClassL,FwLab2,BwLab2,FwLabTabL,BwLabTabL),
  !,
  build_fwbw_class(ToTryL,[Q2|ClassL],UnusedL,FwLabTabL,BwLabTabL,ComplClassL,AllUnusedL),
  !.

build_fwbw_class([Q2|ToTryL],ClassL,UnusedL,FwLabTabL,BwLabTabL,ComplClassL,AllUnusedL) :-
  build_fwbw_class(ToTryL,ClassL,[Q2|UnusedL],FwLabTabL,BwLabTabL,ComplClassL,AllUnusedL),
  !.

build_fwbw_class([],ClassL,UnusedL,_,_,ClassL,UnusedL).

%-------------------------------------------------------------------------------

build_fwbw_classes([Q|UnclassifiedL],FwLabTabL,BwLabTabL,[ClassL|PartitionL]) :-
  !,
  build_fwbw_class(UnclassifiedL,[Q],[],FwLabTabL,BwLabTabL,ClassL,UnusedL),
  build_fwbw_classes(UnusedL,FwLabTabL,BwLabTabL,PartitionL),
  !.

build_fwbw_classes([],_,_,[]).

%-------------------------------------------------------------------------------

fwbw_partition(NQConc,FwLabTabL,BwLabTabL,PartitionL) :-
  gen_list_of_n_states(0,NQConc,QConcL),
  build_fwbw_classes(QConcL,FwLabTabL,BwLabTabL,PartitionL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstracting automata by collapsing their states wrt. forward and backward languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstr_fwbw(Conc,Abstr) :-
  pred_lang([Pred]),
  %% write('Forward labelling wrt. the bad automaton...'),nl,
  compute_uni_forw_labels(Conc,Pred,FwLabTabL),
  %% write('Backward labelling wrt. the bad automaton...'),nl,
  compute_uni_bckw_labels(Conc,Pred,BwLabTabL),
  Conc=fa(ConcSym,NQConc,ConcIniL,ConcFinL,ConcTrL,[]),
  %% write('Computing the partition...'),nl,
  fwbw_partition(NQConc,FwLabTabL,BwLabTabL,PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. forward and backward languages - a DEBUGGING version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Requires new_pred to be initially asserted and its use enabled in the sources.

abstr_fwbw_debug(State,AbsState) :-
  abstr_fwbw(State,AbsState), check_coll(State,AbsState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. backward languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Computing labelling wrt. backward languages by the important states of one of the predicates being used.

%%% For other predicate modules than r(fsa_preds), a modification of rem_compatible_from_set
%%% is necessary...

compute_bw_labels(fa(PredModule,ConcStN,ConcIniL,_,ConcTrL,_),fa(PredModule,_,PredIniL,_,PredTrL,_),ImpStL) :-

  ( (PredModule\=r(fsa_preds), !, write('Predicate module fsa_preds required!'),nl,halt); true ),



  ConcStNN is ConcStN-1,
  initial_labels(ConcStNN,ConcIniL,PredIniL,LabTabLa),
  repeat_step_on(LabTabLa,LabTabLa,LabTabL,ConcTrL,PredTrL),
  %% write('A (not filtered) set of backward labels: '),write(LabTabL),nl,
  add_labels(LabTabL,ImpStL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstracting automata by collapsing their states wrt. backward languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% If always all predicate states are considered in labelling, the first definition
%%% may be commented out.

do_bw_abstr(Conc,[[Pred,ImpStL]|PredL]) :-
  !,
  compute_bw_labels(Conc,Pred,ImpStL),
  do_bw_abstr(Conc,PredL),
  !.

do_bw_abstr(Conc,[Pred|PredL]) :-
  compute_bw_labels(Conc,Pred,all),
  do_bw_abstr(Conc,PredL),
  !.

do_bw_abstr(_,[]) :- !.

%-------------------------------------------------------------------------------



abstr_bw(Conc,Abstr) :-
  pred_lang(PredL), %% Choose this or vvv
  %% pred_lang(PredLa), ( (PredLa=[], !, empty(E),PredL=[E]); PredL=PredLa ), %% Choose this or ^^^
  %% write('Labelling wrt. the particular predicate automata...'),nl,
  do_bw_abstr(Conc,PredL),
  %% write('Computing the partition...'),nl,
  collect_partition(PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  Conc=fa(ConcSym,_,ConcIniL,ConcFinL,ConcTrL,_),
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. backward languages - a DEBUGGING version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Requires new_pred to be initially asserted and its use enabled in the sources.

abstr_bw_debug(State,AbsState) :-
  abstr_bw(State,AbsState), check_coll(State,AbsState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prefix states of a predicate automaton to be taken into account when collapsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

common_prefix(fa(PredModule,ConcStN,ConcIniL,_,ConcTrL,_),fa(PredModule,_,PredIniL,_,PredTrL,_),ImpStL) :-
  ConcStNN is ConcStN-1,
  initial_labels(ConcStNN,ConcIniL,PredIniL,LabTabLa),
  repeat_step_on(LabTabLa,LabTabLa,LabTabL,ConcTrL,PredTrL),
  important_pred_prefix(LabTabL,[],ImpStL).

%%-------------------------------------------------------------------------------

important_pred_prefix([[_,L]|LabTabL],KnownL1,KnownL3) :-
  comp_imp_pred_prefix(L,KnownL1,KnownL2),
  important_pred_prefix(LabTabL,KnownL2,KnownL3),
  !.

important_pred_prefix([],KnownL,KnownL).

%%-------------------------------------------------------------------------------

comp_imp_pred_prefix([Q|L],KnownL1,KnownL3) :-
  insert_to_ord(Q,KnownL1,KnownL2),
  comp_imp_pred_prefix(L,KnownL2,KnownL3),
  !.

comp_imp_pred_prefix([],KnownL,KnownL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reducing a predicate to the important prefix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Note: RedPred is not in the canonical FSA representation!

reduce_prefix(Pred,ImpStL,RedPred) :-
  Pred=fa(PrMod,NQ,IniL,_,TrL,[]),
  filter_trans(ImpStL,TrL,NewTrL),
  length(TrL,TN1),length(NewTrL,TN2),write('A transition reduction: '),write(TN1),write('->'),write(TN2),nl,
  %% fsa_construct_rename_states(PrMod,IniL,ImpStL,NewTrL,[],RedPred). %% Choose this or vvv
  RedPred=fa(PrMod,NQ,IniL,ImpStL,NewTrL,[]). %% Choose this or ^^^

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. 1-letter forward and backward languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% NOT FINISHED: initial/final and not-initial/final should not be collapsed...

empty_table(N,[[NN,[]]|TabLa]) :-
  N>0,
  !,
  NN is N-1,
  empty_table(NN,TabLa),
  !.

empty_table(0,[]).

%-------------------------------------------------------------------------------

do_compute_1l_fwbw_labels(PQ1,PA,PQ2,[trans(CQ1,CA,CQ2)|ConcTrL],FwTbL,BwTbL,NewFwTbL,NewBwTbL) :-
  fsa_preds:conjunction(PA,CA,I),
  ( (I\=not_in([_|_])); (write('Error: not_in appeared in do_compute_1l_fwbw_labels !'),nl,halt) ),
  !,
  add_to_set_of(PQ1,CQ1,FwTbL,NewFwTbLa),
  add_to_set_of(PQ2,CQ2,BwTbL,NewBwTbLa),
  do_compute_1l_fwbw_labels(PQ1,PA,PQ2,ConcTrL,NewFwTbLa,NewBwTbLa,NewFwTbL,NewBwTbL),
  !.

do_compute_1l_fwbw_labels(PQ1,PA,PQ2,[_|ConcTrL],FwTbL,BwTbL,NewFwTbL,NewBwTbL) :-
  do_compute_1l_fwbw_labels(PQ1,PA,PQ2,ConcTrL,FwTbL,BwTbL,NewFwTbL,NewBwTbL),
  !.

do_compute_1l_fwbw_labels(_,_,_,[],FwTbL,BwTbL,FwTbL,BwTbL).

%-------------------------------------------------------------------------------

compute_1l_fwbw_labels([trans(PQ1,PA,PQ2)|PredTrL],ConcTrL,FwTbL,BwTbL,NewFwTbL,NewBwTbL) :-
  !,
  do_compute_1l_fwbw_labels(PQ1,PA,PQ2,ConcTrL,FwTbL,BwTbL,NewFwTbLa,NewBwTbLa),
  compute_1l_fwbw_labels(PredTrL,ConcTrL,NewFwTbLa,NewBwTbLa,NewFwTbL,NewBwTbL),
  !.

compute_1l_fwbw_labels([],_,FwTbL,BwTbL,FwTbL,BwTbL).

%-------------------------------------------------------------------------------

abstr_1l_fwbw(Conc,Abstr) :-
  pred_lang([Pred]),
  Conc=fa(ConcSym,NQConc,ConcIniL,ConcFinL,ConcTrL,[]),
  Pred=fa(_,_,PredIniL,PredFinL,PredTrL,[]),
  write('One letter Fw/Bw labelling wrt. the bad automaton...'),nl,
  empty_table(NQConc,EmptyTabL),
  compute_1l_fwbw_labels(PredTrL,ConcTrL,EmptyTabL,EmptyTabL,FwLabTabL,BwLabTabL),
  write('Computing the partition...'),nl,
  fwbw_partition(NQConc,FwLabTabL,BwLabTabL,PartitionL),
  write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by classical language operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstracting by single languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sngl_abstr(P,used) :-
  state(State),
  empty_intr(State,P), !,
  fsa_regex_compile(complement(fa(P)),NP),
  retract(abs_state(AbsState1)),
  fsa_regex_compile(intersect(fa(NP),fa(AbsState1)),AbsState2),
  %% AbsState2=fa(_,NS,_,_,T,_),length(T,NT),
  %% write('Abstraction refined to: '),write(NS),write(','),write(NT),nl,
  assertz(abs_state(AbsState2)), !.

sngl_abstr(P,used) :-
  state(State),
  fsa_regex_compile(complement(fa(P)),NP),
  empty_intr(State,NP), !,
  retract(abs_state(AbsState1)),
  fsa_regex_compile(intersect(fa(P),fa(AbsState1)),AbsState2),
  %% AbsState2=fa(_,NS,_,_,T,_),length(T,NT),
  %% write('Abstraction refined to: '),write(NS),write(','),write(NT),nl,
  assertz(abs_state(AbsState2)), !.

sngl_abstr(_,unused).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstracting by tuples of languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_pos_neg([],AlrGen) :-
  state(State),
  fsa_regex_compile(complement(fa(AlrGen)),NP),
  empty_intr(State,NP), !,
  retract(abs_state(AbsState1)),
  fsa_regex_compile(intersect(fa(AlrGen),fa(AbsState1)),AbsState2),
  %% AbsState2=fa(_,NS,_,_,T,_),length(T,NT),
  %% write('Abstraction refined to: '),write(NS),write(','),write(NT),nl,
  assertz(abs_state(AbsState2)), !,
  fail.

gen_pos_neg([],_) :- !, fail.

gen_pos_neg([X|GenFromL],AlrGen) :-
  ( (fsa_regex_compile(union(fa(X),fa(AlrGen)),NewGen),
     gen_pos_neg(GenFromL,NewGen));
    (fsa_regex_compile(union(complement(fa(X)),fa(AlrGen)),NewGen),
     gen_pos_neg(GenFromL,NewGen)) ).

%-------------------------------------------------------------------------------

generate_tuples(0,_,AlrGenL) :- !, empty(E),gen_pos_neg(AlrGenL,E).

generate_tuples(_,[],_) :- !, fail.

generate_tuples(ToGen,[X|GenFromL],AlrGenL) :-
  NewToGen is ToGen-1,
  ( generate_tuples(NewToGen,GenFromL,[X|AlrGenL]);
    generate_tuples(ToGen,GenFromL,AlrGenL) ).

%-------------------------------------------------------------------------------

tupl_abstr(N,Max,_) :- N>Max, !.

tupl_abstr(N,Max,BaseL) :-
  write('Abstracting by '),write(N),write('-tuples ...'),nl,
  ( (generate_tuples(N,BaseL,[]));
    (true) ),
  NN is N+1,
  tupl_abstr(NN,Max,BaseL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main part of abstracting states by classical language operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_abstract_lang([P|PredLangL],UnusedL) :-
  sngl_abstr(P,Result),
  ( (Result=used, !,do_abstract_lang(PredLangL,UnusedL));
    (do_abstract_lang(PredLangL,[P|UnusedL])) ), !.

do_abstract_lang([],UnusedL) :-
  length(UnusedL,NUnused),
  write('The number of non-singleton pred. lang.: '),write(NUnused),nl,
  max_for_appl_tuples(MaxForTpl),
  NUnused>MaxForTpl, !.

do_abstract_lang([],UnusedL) :-
  max_tuple(Max),
  tupl_abstr(2,Max,UnusedL).

%-------------------------------------------------------------------------------

abstr_lang(State,AbsState) :-
  pred_lang(PredLangL),
  sig_star(SigStar),
  assertz(state(State)),
  assertz(abs_state(SigStar)),
  write('Abstracting post by single languages...'),nl,
  do_abstract_lang(PredLangL,[]),
  retract(state(State)),
  retract(abs_state(AbsState)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. languages of words up to a certain length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_in_class(Q,C,[[C,QL]|_]) :- member(Q,QL), !.

fl_in_class(Q,C,[_|CL]) :- fl_in_class(Q,C,CL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_add_reach_class_q(A,C,[[A,ClL]|SymClL],[[A,NewClL]|SymClL]) :-
  add_to_set(C,ClL,NewClL),
  !.

fl_add_reach_class_q(A1,C,[[A2,ClL]|SymClL],[[A2,ClL]|NewSymClL]) :-
  fl_add_reach_class_q(A1,C,SymClL,NewSymClL),
  !.

fl_add_reach_class_q(A,C,[],[[A,[C]]]).

%-------------------------------------------------------------------------------

fl_add_reach_class(Q,A,C,[[Q,SymClL]|ReachClL],[[Q,NewSymClL]|ReachClL]) :-
  fl_add_reach_class_q(A,C,SymClL,NewSymClL),
  !.

fl_add_reach_class(Q1,A,C,[[Q2,SymClL]|ReachClL],[[Q2,SymClL]|NewReachClL]) :-
  fl_add_reach_class(Q1,A,C,ReachClL,NewReachClL),
  !.

fl_add_reach_class(Q,A,C,[],[[Q,[[A,[C]]]]]).

%-------------------------------------------------------------------------------

fl_add_reach_classes(Q,[A|AL],C,ReachClL,NewReachClL2) :-
  fl_add_reach_class(Q,A,C,ReachClL,NewReachClL1),
  fl_add_reach_classes(Q,AL,C,NewReachClL1,NewReachClL2),
  !.

fl_add_reach_classes(_,[],_,ReachClL,ReachClL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



fl_reach_classes_t(Q1,X,Q2,SigL,ClL,ReachClL,NewReachClL) :-
  fsa_preds:conjunction(X,in(SigL),I),
  !,
  ( (I=in(IL), !);
    (member(I,SigL), !, IL=[I]);
    (write('Error: unexpected result of the conjunction in fl_reach_classes_t:'),write(I),nl,halt) ),
  member([C,QL],ClL),member(Q2,QL),
  fl_add_reach_classes(Q1,IL,C,ReachClL,NewReachClL),
  !.
fl_reach_classes_t(_,_,_,_,_,ReachClL,ReachClL).

%-------------------------------------------------------------------------------

fl_reach_classes(ReachQL,SigL,[trans(Q1,X,Q2)|TrL],ClL,ReachClL,NewReachClL2) :-
  member(Q2,ReachQL),
  !,
  fl_reach_classes_t(Q1,X,Q2,SigL,ClL,ReachClL,NewReachClL1),
  fl_reach_classes(ReachQL,SigL,TrL,ClL,NewReachClL1,NewReachClL2),
  !.

fl_reach_classes(ReachQL,SigL,[_|TrL],ClL,ReachClL,NewReachClL) :-
  fl_reach_classes(ReachQL,SigL,TrL,ClL,ReachClL,NewReachClL),
  !.

fl_reach_classes(_,_,[],_,ReachClL,ReachClL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_sort_sym_class([X,L1],[X,L2]) :- sort(L1,L2).

%-------------------------------------------------------------------------------

fl_prepare_new_classes(N,QN,OldClL,ReachClL,[StDscr|PrepClL]) :-
  N<QN,
  !,
  member([OldC,OldQL],OldClL),member(N,OldQL),
  ( (member([N,SymClL],ReachClL),
     !,
     maplist(fl_sort_sym_class,SymClL,SymClL1),
     sort(SymClL1,SymClL2),
     StDscr=[OldC,SymClL2,N]);
    (StDscr=[OldC,[],N]) ),
  NN is N+1,
  fl_prepare_new_classes(NN,QN,OldClL,ReachClL,PrepClL),
  !.

fl_prepare_new_classes(QN,QN,_,_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_do_coll_classes(C,X,Y,[[X,Y,Q]|PrepClL],[Q|NewClass],NewClassL) :-
  !,
  fl_do_coll_classes(C,X,Y,PrepClL,NewClass,NewClassL),
  !.

fl_do_coll_classes(C,_,_,[[X,Y,Q]|PrepClL],[],[[CC,[Q|NewClass]]|NewClassL]) :-
  !,
  CC is C+1,
  fl_do_coll_classes(CC,X,Y,PrepClL,NewClass,NewClassL),
  !.

fl_do_coll_classes(_,_,_,[],[],[]).

%-------------------------------------------------------------------------------

fl_coll_classes(PrepClL,[[0,[Q|NewClass1]]|NewClassL1]) :-
  PrepClL=[[X,Y,Q]|PrepClL1],
  fl_do_coll_classes(0,X,Y,PrepClL1,NewClass1,NewClassL1),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_take_fst([X,Y],X).

%-------------------------------------------------------------------------------

fl_comp_new_reach(ReachClL,ReachL) :- maplist(fl_take_fst,ReachClL,ReachL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_comp_new_classes(QN,ReachQL,SigL,TrL,OldClL,NewReachL,NewClassL) :-
  fl_reach_classes(ReachQL,SigL,TrL,OldClL,[],ReachClL),
  fl_prepare_new_classes(0,QN,OldClL,ReachClL,PrepClL1),
  sort(PrepClL1,PrepClL2),
  fl_coll_classes(PrepClL2,NewClassL),
  fl_comp_new_reach(ReachClL,NewReachL),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_comp_nonf(N,QN,FL,NonFL) :-
  N<QN,
  member(N,FL),
  !,
  NN is N+1,
  fl_comp_nonf(NN,QN,FL,NonFL),
  !.

fl_comp_nonf(N,QN,FL,[N|NonFL]) :-
  N<QN,
  !,
  NN is N+1,
  fl_comp_nonf(NN,QN,FL,NonFL),
  !.

fl_comp_nonf(QN,QN,_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% An optimization - we stop sooner than at Lim when ReachQL=NewReachQL and OldClL=NewClL...

fl_comp_classes(N,Lim,QN,ReachQL,SigL,TrL,OldClL,ClL) :-
  N<Lim,
  !,
  NN is N+1,
  fl_comp_new_classes(QN,ReachQL,SigL,TrL,OldClL,NewReachQL,NewClL),
  ( (ReachQL=NewReachQL,OldClL=NewClL, !, ClL=NewClL);
    (fl_comp_classes(NN,Lim,QN,NewReachQL,SigL,TrL,NewClL,ClL)) ),
  !.

fl_comp_classes(Lim,Lim,_,_,_,_,ClL,ClL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_take_snd([X,Y],Y).

%-------------------------------------------------------------------------------



fl_comp_partition(N,QN,FL,TrL,PartL) :-
  sigma(SigL),
  fl_comp_nonf(0,QN,FL,NonFL),
  fl_comp_classes(0,N,QN,FL,SigL,TrL,[[0,FL],[1,NonFL]],ClL),
  maplist(fl_take_snd,ClL,PartL),
  !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstr_fl(Conc,Abstr) :-
  Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
  fin_lang_up_to(Lim),
  write('Computing the partition wrt. languages up to '),write(Lim),write(' letters...'),nl,
  fl_comp_partition(Lim,ConcQN,ConcFinL,ConcTrL,PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%% abstr_fl(Conc,Abstr) :-
%% Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
%% fin_lang_up_to(Lim),
%% write('Computing the partition wrt. languages up to '),write(Lim),write(' letters...'),nl,
%% collapse(Lim,Conc,Abstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. backward languages of words up to a certain length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fb_reach_classes(ReachQL,SigL,[trans(Q1,X,Q2)|TrL],ClL,ReachClL,NewReachClL2) :-
  member(Q1,ReachQL),
  !,
  fl_reach_classes_t(Q2,X,Q1,SigL,ClL,ReachClL,NewReachClL1),
  fb_reach_classes(ReachQL,SigL,TrL,ClL,NewReachClL1,NewReachClL2),
  !.

fb_reach_classes(ReachQL,SigL,[_|TrL],ClL,ReachClL,NewReachClL) :-
  fb_reach_classes(ReachQL,SigL,TrL,ClL,ReachClL,NewReachClL),
  !.

fb_reach_classes(_,_,[],_,ReachClL,ReachClL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fb_comp_new_classes(QN,ReachQL,SigL,TrL,OldClL,NewReachL,NewClassL) :-
  fb_reach_classes(ReachQL,SigL,TrL,OldClL,[],ReachClL),
  fl_prepare_new_classes(0,QN,OldClL,ReachClL,PrepClL1),
  sort(PrepClL1,PrepClL2),
  fl_coll_classes(PrepClL2,NewClassL),
  fl_comp_new_reach(ReachClL,NewReachL),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% An optimization - we stop sooner than at Lim when ReachQL=NewReachQL and OldClL=NewClL...

fb_comp_classes(N,Lim,QN,ReachQL,SigL,TrL,OldClL,ClL) :-
  N<Lim,
  !,
  NN is N+1,
  fb_comp_new_classes(QN,ReachQL,SigL,TrL,OldClL,NewReachQL,NewClL),
  ( (ReachQL=NewReachQL,OldClL=NewClL, !, ClL=NewClL);
    (fb_comp_classes(NN,Lim,QN,NewReachQL,SigL,TrL,NewClL,ClL)) ),
  !.

fb_comp_classes(Lim,Lim,_,_,_,_,ClL,ClL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



fb_comp_partition(N,QN,IL,TrL,PartL) :-
  sigma(SigL),
  fl_comp_nonf(0,QN,IL,NonIL), %% NonIL for fb...
  fb_comp_classes(0,N,QN,IL,SigL,TrL,[[0,IL],[1,NonIL]],ClL),
  maplist(fl_take_snd,ClL,PartL),
  !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstr_fb(Conc,Abstr) :-
  Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
  fin_lang_up_to(Lim),
  write('Computing the partition wrt. backward languages up to '),write(Lim),write(' letters...'),nl,
  fb_comp_partition(Lim,ConcQN,ConcIniL,ConcTrL,PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optimized abstraction by collapsing wrt. backward languages of words up to a certain length
% - We just collapse the states that are more than n steps from the initial state.
% - Applicable to deterministic automata only!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fbo_step_from(From,[trans(From,_,To)|TrL],KnownQL,NewKnownQL,NewQL,UnusedTrL) :-
  !,
  ( (member(To,KnownQL), !, fbo_step_from(From,TrL,KnownQL,NewKnownQL,NewQL,UnusedTrL));
    (fbo_step_from(From,TrL,[To|KnownQL],NewKnownQL,AddedQL,UnusedTrL),NewQL=[To|AddedQL]) ),
  !.

fbo_step_from(From,[trans(X,A,Y)|TrL],KnownQL,NewKnownQL,NewQL,[trans(X,A,Y)|UnusedTrL]) :-
  !,
  fbo_step_from(From,TrL,KnownQL,NewKnownQL,NewQL,UnusedTrL),
  !.

fbo_step_from(_,[],KnownQL,KnownQL,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fbo_step_from_level(_,0,_,KnownQL,_,KnownQL) :- !.

fbo_step_from_level([From|FromQL],Level,TrL,KnownQL,NextFromQL,NewKnownQL2) :-
  fbo_step_from(From,TrL,KnownQL,NewKnownQL1,NewQL,UnusedTrL),
  append(NextFromQL,NewQL,NewNextFromQL),
  fbo_step_from_level(FromQL,Level,UnusedTrL,NewKnownQL1,NewNextFromQL,NewKnownQL2),
  !.

fbo_step_from_level([],Level,TrL,KnownQL,NextFromQL,NewKnownQL) :-
  NewLevel is Level-1,
  fbo_step_from_level(NextFromQL,NewLevel,TrL,KnownQL,[],NewKnownQL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fbo_gen_classes(M,N,KnownQL,[[M]|IndivClL],CollL) :-
  M < N,
  member(M,KnownQL),
  !,
  MM is M+1,
  fbo_gen_classes(MM,N,KnownQL,IndivClL,CollL),
  !.


fbo_gen_classes(M,N,KnownQL,IndivClL,[M|CollL]) :-
  M < N,
  !,
  MM is M+1,
  fbo_gen_classes(MM,N,KnownQL,IndivClL,CollL),
  !.

fbo_gen_classes(N,N,_,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstr_fbo(Conc,Abstr) :-
  Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
  fin_lang_up_to(Lim),
  write('Computing the partition wrt. backward languages up to '),write(Lim),
  write(' letters (optimized computation)...'),nl,
  fbo_step_from_level(ConcIniL,Lim,ConcTrL,ConcIniL,[],KnownQL),
  fbo_gen_classes(0,ConcQN,KnownQL,IndivClL,CollL),
  PartitionL=[CollL|IndivClL],
  %% write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. traces up to a certain length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_ft_gen_st_list(N,[N|L]) :-
  N>0,
  !,
  NN is N-1,
  do_ft_gen_st_list(NN,L),
  !.

do_ft_gen_st_list(0,[0]).

%...............................................................................

ft_gen_st_list(N,L) :-
  NN is N-1,
  do_ft_gen_st_list(NN,L).

%-------------------------------------------------------------------------------



ft_comp_partition(N,QN,TrL,PartL) :-
  sigma(SigL),
  ft_gen_st_list(QN,QL),
  fl_comp_classes(0,N,QN,QL,SigL,TrL,[[0,QL]],ClL),
  maplist(fl_take_snd,ClL,PartL),
  !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstr_ft(Conc,Abstr) :-
  Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
  fin_lang_up_to(Lim),
  write('Computing the partition wrt. traces up to '),write(Lim),write(' letters...'),nl,
  ft_comp_partition(Lim,ConcQN,ConcTrL,PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%%% ((Conc=Abstr,!); (fsa_write_file(zz1,Conc), fsa_write_file(zz2,Abstr))), trace.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. finite length forward languages refined
% by traces up to one. (For a similar version see: abstr_fl_ft).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



abstr_fl_refby_ft(Conc,Abstr) :-
  Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
  fin_lang_up_to(Lim),
  write('Computing the FL partition wrt. languages up to '),write(Lim),write(' letters...'),nl,
  sigma(SigL),
  fl_comp_nonf(0,ConcQN,ConcFinL,NonFL),
  fl_comp_classes(0,Lim,ConcQN,ConcFinL,SigL,ConcTrL,[[0,ConcFinL],[1,NonFL]],ClL1),
  N=1,
  write('Computing the FT partition wrt. languages up to '),write(N),write(' letters...'),nl,
  ft_gen_st_list(ConcQN,QL),
  fl_comp_classes(0,N,ConcQN,QL,SigL,ConcTrL,ClL1,ClL2),
  maplist(fl_take_snd,ClL2,PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. backward traces up to a certain length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



ftb_comp_partition(N,QN,TrL,PartL) :-
  sigma(SigL),
  ft_gen_st_list(QN,QL),
  fb_comp_classes(0,N,QN,QL,SigL,TrL,[[0,QL]],ClL),
  maplist(fl_take_snd,ClL,PartL),
  !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstr_ftb(Conc,Abstr) :-
  Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
  fin_lang_up_to(Lim),
  write('Computing the partition wrt. backward traces up to '),write(Lim),write(' letters...'),nl,
  ftb_comp_partition(Lim,ConcQN,ConcTrL,PartitionL),
  %% write('Partition: '),write(PartitionL),nl,
  adjust_trans(PartitionL,ConcTrL,NewConcTrL),
  adjust_st(PartitionL,ConcFinL,NewConcFinL),
  adjust_st(PartitionL,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Skipping an abstraction step
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The parameters are StepNo and ConcSt... Choose one of the variants.

%% skip_abstr(StepNo,_) :-
%% X is StepNo mod 2,
%% X \= 0,
%% write('... SKIPPING abstraction!'),nl.

skip_abstr(_,fa(_,_,_,_,TrL,_)) :-
  length(TrL,X),
  X < 10,
  write('... SKIPPING abstraction!'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A support for computing R* instead of R*(I)...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fsa_to_fst(fa(r(fsa_frozen),QN,IL,FL,TrL,JL),fa(t(fsa_frozen,fsa_frozen),QN,IL,FL,TrL,JL)).

%-------------------------------------------------------------------------------

fst_to_fsa(fa(t(fsa_frozen,fsa_frozen),QN,IL,FL,TrL,JL),fa(r(fsa_frozen),QN,IL,FL,TrL,JL)).

fst_to_minfsa(fa(t(fsa_frozen,fsa_frozen),QN,IL,FL,TrL,JL),FA2) :-
  FA1=fa(r(fsa_frozen),QN,IL,FL,TrL,JL),
  fsa_regex_compile(mb(fa(FA1)),FA2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main algorithm of A.S.M.C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unite_predicates(PredL,Pred) :-
  add_fa_everywhere(PredL,FaPredL),
  fsa_regex_compile(set(FaPredL),Pred).

%...............................................................................

add_fa_everywhere([A|L],[fa(A)|FaL]) :- add_fa_everywhere(L,FaL).

add_fa_everywhere([],[]).

%-------------------------------------------------------------------------------

augment_summary_bad(NewBad) :-
  retract(sum_bad(OldSumBad)),
  fsa_regex_compile(union(fa(OldSumBad),fa(NewBad)),NewSumBad),
  assertz(sum_bad(NewSumBad)).

%-------------------------------------------------------------------------------

refine_abstraction(CritConcState,NewPred) :-
  run(N),
  NewPred=fa(_,Size,_,_,_,_),
  write('In run '),write(N),write(', adding a new pred. aut. with '),write(Size),write(' states.'),nl,
  name(N,NL),atom_chars(PN,NL),atom_concat('npr',PN,PP),fsa_write_file(PP,NewPred),
  %%% !!!!! BEWARE OF USING THE RIGHT VERSION OF prepare_summary_bad !!!!!
  %%% Use summ_bad:-pred_lang if both of the following hold:
  %%% (1) New predicates are being united with the old ones.
  %%% (2) Only the set of bad states is used as the initial predicate.
  %%% !!!!! BEWARE OF SWITCHING THIS CORRECTLY !!!!!



  %-------------
  %-------------
  %% unite_predicates([NewPred|PredLangL],UniPred), %% Choose this or ...
  %% assertz(pred_lang([UniPred])),
  %% %% augment_summary_bad(NewPred), %% Switch off when summ_bad:-pred_lang is used
  %-------------
  %-------------
  %% common_prefix(CritConcState,NewPred,ImpStL), %% Choose this or ...
  %% write('These are the important states of the new predicate: '),write(ImpStL),nl,
  %% reduce_prefix(NewPred,ImpStL,NewRedPred),
  %% assertz(pred_lang([NewRedPred|PredLangL])),
  %% augment_summary_bad(NewPred), %% May be switched off.
  %-------------
  %-------------
  %-------------
  %% common_suffix(CritConcState,NewPred,ImpStL), %% Choose this or ...
  %% write('These are the important states of the new predicate: '),write(ImpStL),nl,
  %% reduce_suffix(NewPred,ImpStL,NewRedPred),
  %% assertz(pred_lang([NewRedPred|PredLangL])),
  %% augment_summary_bad(NewPred), %% May be switched off.
  %-------------
  %-------------
  %% common_suffix(CritConcState,NewPred,ImpStL), %% Choose this or ...
  %% write('These are the important states of the new predicate: '),write(ImpStL),nl,
  %% NewPred=fa(PrMod,Size,_,FinL,TrL,[]),
  %% RedPred=fa(PrMod,Size,ImpStL,FinL,TrL,[]),
  %% unite_predicates([RedPred|PredLangL],UniPred),
  %% assertz(pred_lang([UniPred])),
  %% %% augment_summary_bad(NewPred), %% Switch off when summ_bad:-pred_lang is used
  %-------------







  %-------------







  %-------------







  %-------------
  %-------------
  %-------------
  %% retract(fin_lang_up_to(OldLim)), %% !!! Switch off retract(pred_lang(PredLangL)) %% Choose this or ^^^
  %% %% NewLim is OldLim+Size, %% Choose this or vvv
  %% %% NewLim is OldLim+(Size//2), %% Choose this or ^^^
  %% write('Increasing the limit on the lenghts of the words considered: '),write(OldLim),write(' -> '),write(NewLim),nl,
  %% assertz(fin_lang_up_to(NewLim)),
  %% augment_summary_bad(NewPred), %% May be switched off.
  %-------------

  retract(unwound_times(UnwoundTimes)),
  NewUnwoundTimes is UnwoundTimes+1,
  assertz(unwound_times(NewUnwoundTimes)),

  %-------------





  %-------------
  %-------------
  %% retract(new_pred(NewPredLangL)),assertz(new_pred([NewPred|NewPredLangL])),
  %% add_self_loops(NewPred,NewPredSelfLoops), %% Adding self-loops everywhere
  %% retract(pred_lang(PredLangL)),assertz(pred_lang([NewPredSelfLoops|PredLangL])),
  %% ( (with_self_loop_acclr, !,try_self_loop_acclr(N,[NewPred|NewPredLangL])); true ),
  !.

%-------------------------------------------------------------------------------

%%% To be used with trans. rel. embedding stuttering (suitable for safety):

do_one_run(N,AbsReach,Result,Value) :-
  tr(TR),
  NN is N+1,
  write('Computing post...'),nl,
  AbsReach=fa(_,NS1,_,_,T1,_),length(T1,NT1),
  write('Abstract source automaton: '),write(NS1),write(','),write(NT1),nl,

  fsa_regex_compile(mb(range(compose(fa(AbsReach),fa(TR)))),NewReach),
    %% mmdff(AbsReach,MmdffOld),write('MmdffOld: '),write(MmdffOld),write(', '),
    %% mmdff(NewReach,MmdffNew),write('MmdffNew: '),write(MmdffNew),nl,





  NewReach=fa(_,NS2,_,_,T2,_),length(T2,NT2),
  write('Concrete target automaton: '),write(NS2),write(','),write(NT2),nl,
  sum_bad(Bad),
  write('Intersecting with bad states...'),nl,
  fsa_regex_compile(intersect(fa(NewReach),fa(Bad)),ImmReachBad),



  ( (empty(ImmReachBad), !,do_one_run_ph2(NN,AbsReach,NewReach,Result,Value));
    (Result=bad,Value=[ImmReachBad,ImmReachBad,NewReach],write('XXXXXXXXX'),nl) ).

%%% To be used with trans. rel. NOT embedding stuttering (suitable for liveness):

%% do_one_run(N,AbsReach,Result,Value) :-
%% tr(TR),
%% NN is N+1,
%% write('Computing post...'),nl,
%% AbsReach=fa(_,NS1,_,_,T1,_),length(T1,NT1),
%% write('Abstract source automaton: '),write(NS1),write(','),write(NT1),nl,
%% fsa_regex_compile(mb(range(compose(fa(AbsReach),fa(TR)))),OneStepReach),
%% OneStepReach=fa(_,NS2,_,_,T2,_),length(T2,NT2),
%% write('Concrete target automaton: '),write(NS2),write(','),write(NT2),nl,
%% sum_bad(Bad),
%% write('Intersecting with bad states...'),nl,
%% fsa_regex_compile(intersect(fa(OneStepReach),fa(Bad)),ImmReachBad),
%% #ifdef GenErrReached
%% fsa_write_file('error_reached',ImmReachBad),
%% #endif
%% ( (empty(ImmReachBad),
%% !,
%% fsa_regex_compile(union(fa(AbsReach),fa(OneStepReach)),NewReach),
%% do_one_run_ph2(NN,AbsReach,NewReach,Result,Value));
%% %% (empty(ImmReachBad), %%% Acceleration only on the new states, then a union with the old ones.
%% %% !,
%% %% do_one_run_ph2(NN,AbsReach,OneStepReach,Result,Value));
%% (Result=bad,Value=[ImmReachBad,ImmReachBad,OneStepReach],write('XXXXXXXXX'),nl) ).

%...............................................................................

iterate_abstract(ConcAtm,AbstrAtm) :-
  abstract(ConcAtm,AbstrAtm1),
  ( (ConcAtm=AbstrAtm1, !, AbstrAtm=AbstrAtm1);
    iterate_abstract(AbstrAtm1,AbstrAtm) ).

%...............................................................................

%%% When checking whether a fixpoint has been reached, we rely on canonicity of the automata encoding!!!!

%% do_one_run_ph2(N,AbsReach,NewReach,Result,Value) :-
%% abstract(NewReach,NewAbsReach),
%% ( (AbsReach=NewAbsReach, !,Result=good,Value=NewAbsReach); %% Returning the fixpoint
%% (write('>>> '),write(N),write(' >>>'),nl,do_one_run_ph3(N,NewReach,NewAbsReach,Result,Value)) ).

%%% Trying to detect a non-empty intersection with bad states immediately after abstraction too.

do_one_run_ph2(N,AbsReach,NewReach,Result,Value) :-
  abstract(NewReach,NewAbsReach), %% Regular abstraction.
  %% ( (skip_abstr(N,NewReach), !, NewAbsReach=NewReach); abstract(NewReach,NewAbsReach)), %% Intermittent abstraction.
  %% iterate_abstract(NewReach,NewAbsReach), %% Iterated abstraction
  sum_bad(Bad),
  write('Intersecting the abstraction with bad states...'),nl,
  fsa_regex_compile(intersect(fa(NewAbsReach),fa(Bad)),AbsImmBad),



  ( (empty(AbsImmBad), !,
     ( (AbsReach=NewAbsReach, !,Result=good,Value=NewAbsReach); %% Returning the fixpoint %% Choose this or vvv
       %% (is_included(NewAbsReach,AbsReach), !,Result=good,Value=NewAbsReach); %% Returning the fixpoint %% Choose this or ^^^
       (write('>>> '),write(N),write(' >>>'),nl,
        %% name(N,NL),atom_chars(PN,NL),atom_concat('step',PN,PP),fsa_write_file(PP,NewAbsReach),
        do_one_run_ph3(N,NewReach,NewAbsReach,Result,Value)) ));
    (write('YYYYYYYYY'),nl,
     do_one_run_ph4(N,NewReach,NewAbsReach,AbsImmBad,NewReach,Result,Value)) ).

%%% Acceleration only on the new states, then a union with the old ones.

%% do_one_run_ph2(N,AbsReach,OneStepReach,Result,Value) :-
%% abstract(OneStepReach,AbsOneStepReach), %% Regular abstraction.
%% fsa_regex_compile(union(fa(AbsReach),fa(AbsOneStepReach)),NewAbsReach),
%% %% ( (skip_abstr(N,NewReach), !, NewAbsReach=NewReach); abstract(NewReach,NewAbsReach)), %% Intermittent abstraction.
%% sum_bad(Bad),
%% write('Intersecting the abstraction with bad states...'),nl,
%% fsa_regex_compile(intersect(fa(NewAbsReach),fa(Bad)),AbsImmBad),
%% #ifdef GenErrReached
%% fsa_write_file('error_reached',ImmReachBad),
%% #endif
%% ( (empty(AbsImmBad), !,
%% ( (AbsReach=NewAbsReach, !,Result=good,Value=NewAbsReach); %% Returning the fixpoint %% Choose this or vvv
%% %% (is_included(NewAbsReach,AbsReach), !,Result=good,Value=NewAbsReach); %% Returning the fixpoint %% Choose this or ^^^
%% (write('>>> '),write(N),write(' >>>'),nl,
%% %% name(N,NL),atom_chars(PN,NL),atom_concat('step',PN,PP),fsa_write_file(PP,NewAbsReach),
%% do_one_run_ph3(N,NewReach,NewAbsReach,Result,Value)) ));
%% (write('YYYYYYYYY'),nl,
%% do_one_run_ph4(N,NewReach,NewAbsReach,AbsImmBad,NewReach,Result,Value)) ).

%...............................................................................

do_one_run_ph3(N,NewReach,NewAbsReach,Result,Value) :-
  do_one_run(N,NewAbsReach,SubResult,SubValue),
  ( (SubResult=good, !,Result=SubResult,Value=SubValue);
    (SubResult=new, !,Result=SubResult,Value=SubValue);
    (itr(ITR),
     SubValue=[NextReachBad1,NextReachBad2,NextReach],
     augment_summary_bad(NextReachBad2), %% Adding big intermediary sets reaching bad
     %% augment_summary_bad(NextReachBad1), %% Adding small intermediary sets reaching bad
     write('Computing pre...'),nl,

     fsa_regex_compile(mb(range(compose(fa(NextReachBad1),fa(ITR)))),SrcBad),





     SrcBad=fa(_,NS1,_,_,T1,_),length(T1,NT1),
     write('After a step back: '),write(NS1),write(','),write(NT1),nl,
     write('Intersecting with reachable...'),nl,
     fsa_regex_compile(intersect(fa(SrcBad),fa(NewAbsReach)),ReachBad),
     ReachBad=fa(_,NS2,_,_,T2,_),length(T2,NT2),
     write('Reachable bad states: '),write(NS2),write(','),write(NT2),nl,
     ( (empty(ReachBad), !,do_one_run_ph4(N,NewReach,NewAbsReach,NextReachBad2,NextReach,Result,Value)); %% A big or small
       %% (empty(ReachBad), !,do_one_run_ph4(N,NewReach,NewAbsReach,NextReachBad1,NextReach,Result,Value)); %% new pred (vvv too)
       (Result=bad,Value=[ReachBad,SrcBad,NewReach],write('<<<<<'),nl) )) ).

%...............................................................................

%%% Do we compute incrementally OR after a refinement, we always restart from the very beginning:
do_one_run_ph4(N,NewReach,NewAbsReach,NewPred,NextReach,Result,Value) :- Result=new,Value=[NextReach,NewPred], !.


%-------------------------------------------------------------------------------

%%% Result: good---property verified, bad---property broken (for SubResult, __may be__ broken),
%%% new---a new predicate has been found
%%% Value: for Result=good, the abstract reachability set,
%%% for Result=bad, the error reached
%%% (for SubResult=bad, SubValue1: reachable error one forward step further obtained from a backwards run,
%%% SubValue2: SubValue1 not intersected with the forward reachable states yet)

one_run(Result,Value) :-
  init(Init),
  write('Abstracting init...'),nl, abstract(Init,AbsInit), %% Choose either this or 'v'
  %% AbsInit=Init, %% Choose either this or '^'
  write('>>> 1 >>>'),nl,
  do_one_run(1,AbsInit,SubResult,SubValue),
  ( (SubResult=good, !,Result=SubResult,Value=SubValue);
    (SubResult=new, !,Result=SubResult,Value=SubValue);
    (itr(ITR),
     SubValue=[NextReachBad1,NextReachBad2,NextReach],
     write('Computing pre...'),nl,

     fsa_regex_compile(mb(range(compose(fa(NextReachBad1),fa(ITR)))),SrcBad),





     SrcBad=fa(_,NS1,_,_,T1,_),length(T1,NT1),
     write('After a step back: '),write(NS1),write(','),write(NT1),nl,
     write('Intersecting with reachable...'),nl,
     fsa_regex_compile(intersect(fa(SrcBad),fa(AbsInit)),ReachBad),
     ReachBad=fa(_,NS2,_,_,T2,_),length(T2,NT2),
     write('After an inters. with reach. states: '),write(NS2),write(','),write(NT2),nl,
     ( (empty(ReachBad), !,Result=new,Value=[NextReach,NextReachBad2]); %% A big or small
       %% (empty(ReachBad), !,Result=new,Value=[NextReach,NextReachBad1]); %% new pred (^^^ too)
       (write('Intersecting with init...'),nl,
        fsa_regex_compile(intersect(fa(SrcBad),fa(Init)),ReallyReachBad),
        ReallyReachBad=fa(_,NS3,_,_,T3,_),length(T3,NT3),
        write('After an inters. with initial states: '),write(NS3),write(','),write(NT3),nl,
        ( (empty(ReallyReachBad), !,Result=new,Value=[Init,SrcBad]);
          (Result=bad,Value=ReallyReachBad) )) )) ).

%-------------------------------------------------------------------------------

do_asmc :-
  retract(run(OldN)),
  N is OldN+1,
  assertz(run(N)),
  write('~~~~~ Run '),write(N),write(':'),nl,
  one_run(Result,Value),
  ( (Result=good, !,write('----- Property holds!'),nl,
     fsa_write_file(reachable,Value) );
    (Result=bad, !,fsa_write_file('error_from',Value),write('----- Property broken!'),nl);
    (Value=[CritConcState,NewPred],refine_abstraction(CritConcState,NewPred),do_asmc) ),
  !.

%-------------------------------------------------------------------------------

prepare_summary_bad :-
  bad(Bad),assertz(sum_bad(Bad)). %% Choose this (together with its manipulation in add_new_pred) or vvv
  %% assertz(:-(sum_bad(Bad),pred_lang([Bad]))). %% Choose this (without the sum_bad manipulation in add_new_pred) or ^^^

%-------------------------------------------------------------------------------

asmc :-
  fsa_regex_atom_compile('{}',Empty),
  assertz(empty(Empty)),
  %% fsa_regex_atom_compile('file(sigma)*',SigStar), %% For the abstraction based on unions/intersections/... of languages only.
  %% assertz(sig_star(SigStar)),
  tr(TR),
  fsa_regex_compile(inverse(fa(TR)),ITR),

  assertz(itr(ITR)),





  good(Good),
  fsa_regex_compile(complement(fa(Good)),Bad),
  assertz(bad(Bad)),
  prepare_summary_bad,
  assertz(run(0)),
  do_asmc.

%...............................................................................

asmc_bad :-
  fsa_regex_atom_compile('{}',Empty),
  assertz(empty(Empty)),
  %% fsa_regex_atom_compile('file(sigma)*',SigStar), %% For the abstraction based on unions/intersections/... of languages only.
  %% assertz(sig_star(SigStar)),
  tr(TR),
  fsa_regex_compile(inverse(fa(TR)),ITR),
  assertz(itr(ITR)),
  prepare_summary_bad,
  assertz(run(0)),
  do_asmc.

%...............................................................................

asmc_rev :-
  fsa_regex_atom_compile('{}',Empty),
  assertz(empty(Empty)),
  %% fsa_regex_atom_compile('file(sigma)*',SigStar), %% For the abstraction based on unions/intersections/... of languages only.
  %% assertz(sig_star(SigStar)),
  itr(ITR),
  fsa_regex_compile(inverse(fa(ITR)),TR),
  assertz(tr(TR)),
  prepare_summary_bad,
  assertz(run(0)),
  do_asmc.

%-------------------------------------------------------------------------------

asmc_time_yap :-
  statistics(cputime,[T1,_]),
  asmc,
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

%...............................................................................

asmc_bad_time_yap :-
  statistics(cputime,[T1,_]),
  asmc_bad,
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

%...............................................................................

asmc_rev_time_yap :-
  statistics(cputime,[T1,_]),
  asmc_rev,
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A simple counterexample-analysis acceleration based on adding self loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Detection of widening situations in 3 steps.

try_self_loop_acclr(N,[P1,P2,P3,P4|_]) :-
  N>3,
  P1=fa(_,Size1,_,_,_,_),
  P2=fa(_,Size2,_,_,_,_),
  P3=fa(_,Size3,_,_,_,_),
  P4=fa(_,Size4,_,_,_,_),
  Delta is Size1-Size2,
  Delta is Size2-Size3,
  Delta is Size3-Size4, !,
  self_loop_acclr(P4,P3,P3acclr),
  P3acclr=fa(_,Size,_,_,_,_),
  write('After counterexample-analysis acceleration in run '), write(N),
  write(', adding a pred. aut. with '),write(Size),write(' states...'),nl,
  name(N,NL),atom_chars(PN,NL),atom_concat('pr_acclr',PN,PP),fsa_write_file(PP,P3acclr),
  %% retract(max_tuple(NPred)),NNPred is NPred+2,assertz(max_tuple(NNPred)),
  retract(pred_lang(PredLangL)),assertz(pred_lang([P3acclr|PredLangL])), !. %% Choose this or vvv
  %% retract(pred_lang(PredLangL)),unite_predicates([P3acclr|PredLangL],UniPred),assertz(pred_lang([UniPred])), !. %% Choose this or ^^^

%% Detection of widening situations in 2 steps.

% try_self_loop_acclr(N,[P2,P3,P4|_]) :-
% N>2,
% P2=fa(_,Size2,_,_,_,_),
% P3=fa(_,Size3,_,_,_,_),
% P4=fa(_,Size4,_,_,_,_),
% Delta is Size2-Size3,
% Delta is Size3-Size4, !,
% self_loop_acclr(P4,P3,P3acclr),
% P3acclr=fa(_,Size,_,_,_,_),
% write('After counterexample-analysis acceleration in run '), write(N),
% write(', adding an abs. aut. with '),write(Size),write(' states...'),nl,
% name(N,NL),atom_chars(PN,NL),atom_concat('pr_acclr',PN,PP),fsa_write_file(PP,P3acclr),
% %% retract(max_tuple(NPred)),NNPred is NPred+2,assertz(max_tuple(NNPred)),
% retract(pred_lang(PredLangL)),assertz(pred_lang([P3acclr|PredLangL])), !. %% Choose this or vvv
% %% retract(pred_lang(PredLangL)),unite_predicates([P3acclr|PredLangL],UniPred),assertz(pred_lang([UniPred])), !. %% Choose this or ^^^

try_self_loop_acclr(_,_).

%-------------------------------------------------------------------------------

self_loop_acclr(A,B1,B2) :-
  A=fa(_,_,[Ai],_,AtL,_),
  B1=fa(_,Bs1,[Bi1],Bf1L,Bt1L,Bj1L),
  assertz(at(AtL)),
  assertz(bt(Bt1L)),
  assertz(loopon([])),
  cmp_from(Ai,Bi1),
  retract(at(_)),
  retract(bt(_)),
  retract(loopon(SL)),
  tr_to_loop(SL,SL,Bt1L,LoopL),
  %% write('Acceleration is adding the following loops: '),write(LoopL),nl,
  append(Bt1L,LoopL,Bt2L),
  fsa_construct(Bs1,[Bi1],Bf1L,Bt2L,Bj1L,B2).

%-------------------------------------------------------------------------------

cmp_from(As,Bs) :-
  retract(bt(Bt1L)),
  find_from(Bs,Bt1L,BstL,Bt2L),
  assertz(bt(Bt2L)),
  cmp_from_using(As,BstL).

cmp_from_using(As1,[trans(Bs1,A,Bs2)|BstL]) :-
  at(AtL),
  ( (member(trans(As1,A,As2),AtL), !,cmp_from(As2,Bs2),cmp_from_using(As1,BstL));
    (retract(loopon(L)),assertz(loopon([Bs1|L]))) ),
  !.

cmp_from_using(_,[]).

%-------------------------------------------------------------------------------

find_from(S1,[trans(S1,A,S2)|TrL1],[trans(S1,A,S2)|TrL2],TrL3) :-
  find_from(S1,TrL1,TrL2,TrL3), !.

find_from(S1,[trans(S2,A,S3)|TrL1],TrL2,[trans(S2,A,S3)|TrL3]) :-
  find_from(S1,TrL1,TrL2,TrL3), !.

find_from(_,_,[],[]).

%-------------------------------------------------------------------------------

do_tr_to_loop(S1,ForbiddenL,[trans(S1,_,S2)|TrL1],TrL2) :-
  member(S2,ForbiddenL), !,
  do_tr_to_loop(S1,ForbiddenL,TrL1,TrL2), !.

do_tr_to_loop(S1,ForbiddenL,[trans(S1,A,_)|TrL1],[trans(S1,A,S1)|TrL2]) :-
  do_tr_to_loop(S1,ForbiddenL,TrL1,TrL2), !.

do_tr_to_loop(S1,ForbiddenL,[_|TrL1],TrL2) :-
  do_tr_to_loop(S1,ForbiddenL,TrL1,TrL2), !.

do_tr_to_loop(_,_,[],[]).

%-------------------------------------------------------------------------------

tr_to_loop([S|SL],ForbiddenL,TrL,TrL3) :-
  do_tr_to_loop(S,ForbiddenL,TrL,TrL1),
  tr_to_loop(SL,ForbiddenL,TrL,TrL2),
  append(TrL1,TrL2,TrL3), !.

tr_to_loop([],_,_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adding self-loops everywhere
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_add_self_loops([trans(S,A,S)|TrL1],[trans(S,A,S)|TrL2]) :-
  do_add_self_loops(TrL1,TrL2), !.

do_add_self_loops([trans(S1,A,S2)|TrL1],[trans(S1,A,S1),trans(S1,A,S2)|TrL2]) :-
  do_add_self_loops(TrL1,TrL2), !.

do_add_self_loops([],[]).

add_self_loops(fa(X,S,I,F,T1,Y),A) :-
  do_add_self_loops(T1,T2),
  A=fa(X,S,I,F,T2,Y).

%% fsa_regex_compile(mb(fa(fa(X,S,I,F,T2,Y))),A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%===============================================================================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some Experiments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing maximum of minimum distances of states from the final states
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mmdff_go_back_from(From,[trans(To,_,From)|TrL],[To|ToL]) :-
  !,
  mmdff_go_back_from(From,TrL,ToL).

mmdff_go_back_from(From,[_|TrL],ToL) :-
  !,
  mmdff_go_back_from(From,TrL,ToL).

mmdff_go_back_from(_,[],[]).

%-------------------------------------------------------------------------------

mmdff_go_back([From|FromL],TransL,ToL) :-
  mmdff_go_back_from(From,TransL,ToL1),
  mmdff_go_back(FromL,TransL,ToL2),
  append(ToL1,ToL2,ToL).

mmdff_go_back([],_,[]).

%-------------------------------------------------------------------------------

mmdff_list1_minus_list2([X|XL],YL,DL) :-
  member(X,YL),
  !,
  mmdff_list1_minus_list2(XL,YL,DL).

mmdff_list1_minus_list2([X|XL],YL,[X|DL]) :-
  !,
  mmdff_list1_minus_list2(XL,YL,DL).

mmdff_list1_minus_list2([],_,[]).

%-------------------------------------------------------------------------------

do_mmdff(Depth,LastReachedL,UnreachedL,TransL,Mmdff) :-
  mmdff_go_back(LastReachedL,TransL,NewLastReachedL),
  mmdff_list1_minus_list2(UnreachedL,NewLastReachedL,NewUnreachedL),
  NewDepth is Depth+1,
  ( (NewUnreachedL=[], !, Mmdff=NewDepth);
    (do_mmdff(NewDepth,NewLastReachedL,NewUnreachedL,TransL,Mmdff)) ).

%-------------------------------------------------------------------------------

mmdff_list_upto(0,[]) :- !.

mmdff_list_upto(N,[NN|L]) :-
  NN is N-1,
  mmdff_list_upto(NN,L).

%-------------------------------------------------------------------------------

mmdff(fa(_,NS,_,FinL,TransL,_),Mmdff) :-
  mmdff_list_upto(NS,StL),
  mmdff_list1_minus_list2(StL,FinL,NonFinL),
  ( (NonFinL=[], !, Mmdff=0);
    (do_mmdff(0,FinL,NonFinL,TransL,Mmdff)) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%===============================================================================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Learning Method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculating a fixpoint over configurations of a limited length (length-preserving systems)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_list_upto(M,N,[]) :- M>N, !.

lrn_list_upto(M,N,[M|L]) :-
  MM is M+1,
  lrn_list_upto(MM,N,L).

%-------------------------------------------------------------------------------

lrn_qtrans_upto(M,N,[]) :- M=N, !.


lrn_qtrans_upto(M,N,[trans(M,not_in([]),MM)|L]) :-
  MM is M+1,
  lrn_qtrans_upto(MM,N,L).
%...............................................................................

lrn_gen_trans([],_,_,[]) :- !.

lrn_gen_trans([X|L],F,T,[trans(F,X,T)|NL]) :- lrn_gen_trans(L,F,T,NL).

%-------------------------------------------------------------------------------

lrn_all_words_upto_n(N,fa(Pred,NN,[0],FinL,TrL,[])) :-

    Pred=r(fsa_preds),



  NN is N+1,
  lrn_list_upto(0,N,FinL),
  lrn_qtrans_upto(0,N,TrL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_one_step_on(From,TR,To) :-

    %%% write('>> One step on... '), nl,
    fsa_regex_compile(mb(range(compose(fa(From),fa(TR)))),To),






  true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_reachable(From,TR,Reachable) :-
  lrn_one_step_on(From,TR,TR_From),
  ( (is_included(TR_From,From), !, Reachable=From); %% Choose this or vvv
    %% (TR_From=From, !, Reachable=From); %% Choose this or ^^^
    lrn_reachable(TR_From,TR,Reachable) ),
  !.

%-------------------------------------------------------------------------------

lrn_reachable_upto_n(Init,N,TR,Reachable) :-
  lrn_all_words_upto_n(N,AllUpToN),
  fsa_regex_compile(intersect(fa(Init),fa(AllUpToN)),InitUpToN),
  lrn_reachable(InitUpToN,TR,Reachable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculating a fixpoint over configurations of a limited length (NON-length-preserving systems)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_do_nlenpr_reachable_upto_n(From,AllUpToN,TR,Reachable) :-
  %%% write('>> One step on... '), nl,
  fsa_regex_compile(mb(range(compose(fa(From),fa(TR)))),TR_From),
  fsa_regex_compile(intersect(fa(TR_From),fa(AllUpToN)),TR_From_Restr),
  ( (is_included(TR_From_Restr,From), !, Reachable=From); %% Choose this or vvv
    %% (TR_From_Restr=From, !, Reachable=From); %% Choose this or ^^^
    lrn_do_nlenpr_reachable_upto_n(TR_From_Restr,AllUpToN,TR,Reachable) ),
  !.

%-------------------------------------------------------------------------------

lrn_nlenpr_reachable_upto_n(From,N,TR,Reachable) :-
  lrn_all_words_upto_n(N,AllUpToN),
  lrn_do_nlenpr_reachable_upto_n(From,AllUpToN,TR,Reachable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some suport for dealing with transducers - computing the alphabet sigma x sigma
% ( May be used when preparing the model... - it is not called from here. )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_do_gen_sigma(_,[],[]) :- !.

lrn_do_gen_sigma(X,[Y|T],[X/Y|NT]) :- lrn_do_gen_sigma(X,T,NT).

%-------------------------------------------------------------------------------

lrn_gen_sigma([],_,[]) :- !.

lrn_gen_sigma([H|T],L,NL) :- lrn_do_gen_sigma(H,L,L1),lrn_gen_sigma(T,L,L2),append(L1,L2,NL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main loop of the learning method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_do_collapse(State,AbsState) :-
  true.

%-------------------------------------------------------------------------------

lrn_try_collapse(Reach,Init,Bad,TR,AbsReach) :-
  lrn_do_collapse(Reach,AbsReach),
  write('-- Checking the inclusion of Init... '), nl,
  is_included(Init,AbsReach), %% Choose this or vvv
  %% fsa_regex_compile(intersect(fa(AbsReach),fa(Init)),Init), %% Choose this or ^^^
  write('-- Checking the fixpoint property... '), nl,
  lrn_one_step_on(AbsReach,TR,AbsReach_TR), %% Choose this or vvv
    %%% write(' >>>'),nl,
    is_included(AbsReach_TR,AbsReach),
  %% fsa_regex_compile(mb(mb(range(compose(fa(AbsReach),fa(TR))))),AbsReach), %% Choose this or ...
  %% fsa_regex_compile(mb(range(compose(fa(AbsReach),fa(TR)))),AbsReach), %% Choose this or ^^^
  write('-- Intersecting with bad...'), nl,
  fsa_regex_compile(intersect(fa(AbsReach),fa(Bad)),BadReach),
  empty(BadReach),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lrn_try_length_N(N,ExitCode,Result) :-
  init(Init),
  tr(TR),
  bad(Bad),
  write('Computing all reachable configurations of length up to: '), write(N), nl,

    lrn_reachable_upto_n(Init,N,TR,Reach),
    %%%% fsa_write_file('lrn_reachable_upto_n',Reach), trace,




  write('- Intersecting with bad...'), nl,
  fsa_regex_compile(intersect(fa(Reach),fa(Bad)),ReachBad),
  ( (empty(ReachBad), !,
     coll_fin_lang_up_to(InitI),
     assertz(fin_lang_up_to(InitI)),
     HalfN is N//2,
     repeat,
       ( (lrn_try_collapse(Reach,Init,Bad,TR,AbsReach), !,
          retract(fin_lang_up_to(_)),
          ExitCode=good,
          Result=AbsReach);
         (retract(fin_lang_up_to(I)),
          ( (I>=HalfN, !, fail); %% Choose this or vvv
            %% (I>=N, !, fail); %% Choose this or ^^^
            (II is I+1,
             assertz(fin_lang_up_to(II)),
             fail ) ) ) ) );
    (ExitCode=bad, Result=ReachBad) ),
  !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_verify :-
  fsa_regex_atom_compile('{}',Empty),
  assertz(empty(Empty)),
  repeat,
    gen_conf_up_to(N),
    ( (lrn_try_length_N(N,ExitCode,Result),
       ( (ExitCode=good, !,write('----- Property holds!'),nl,
          fsa_write_file(reachable,Result) );
         (ExitCode=bad, !,write('----- Property broken!'),nl,
          fsa_write_file('error_reached',Result)) ) );
      (retract(gen_conf_up_to(N)),
       NN is N+1,
       assertz(gen_conf_up_to(NN)),
       fail) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lrn_time_verify_yap :-
  statistics(cputime,[T1,_]),
  lrn_verify,
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trachtenbrot - Barzdin (via extracting languages of states)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb_lang_of_state(N,fa(Pred,NQ,_,FinL,TL,JL),A) :-
  fsa_regex_compile(mb(fa(fa(Pred,NQ,[N],FinL,TL,JL))),A).

%-------------------------------------------------------------------------------

tb_lang_of_states(NQ,fa(_,NQ,_,_,_,_),[]) :- !.

tb_lang_of_states(N,AutIn,[[N,Aut]|AutTab]) :-
  NN is N+1,
  tb_lang_of_states(NN,AutIn,AutTab),
  tb_lang_of_state(N,AutIn,Aut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb_do_try_to_merge_state(StLang,Mask,StLangL,[RedirState|_],RedirState) :-
  member([RedirState,RedirStateLang],StLangL),
  fsa_regex_compile(intersect(fa(RedirStateLang),fa(Mask)),RestrRedirStateLang),
  %% is_included(RestrRedirStateLang,StLang),is_included(StLang,RestrRedirStateLang),
  RestrRedirStateLang=StLang,
  !.

tb_do_try_to_merge_state(StLang,Mask,StLangL,[_|RedirStateL],RedirState) :-
  tb_do_try_to_merge_state(StLang,Mask,StLangL,RedirStateL,RedirState).

%-------------------------------------------------------------------------------

tb_try_to_merge_state(State,Depth,N,StLangL,DoneL,RedirState) :-
  NN is N-Depth,
  lrn_all_words_upto_n(NN,Mask),
  member([State,StLang],StLangL),
  tb_do_try_to_merge_state(StLang,Mask,StLangL,DoneL,RedirState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb_explore_state(_,_,_,_,DoneL,[],[],DoneL,[],[]) :- !.

tb_explore_state(St,D,N,StLangL,DoneL,[trans(St,X,ToSt)|TrL],NewToDoL,NewDoneL,[trans(St,X,RedirSt)|NewTrl],UnusedTrL) :-
  DD is D+1,
  tb_try_to_merge_state(ToSt,DD,N,StLangL,DoneL,RedirSt),
  !,
  tb_explore_state(St,D,N,StLangL,DoneL,TrL,NewToDoL,NewDoneL,NewTrl,UnusedTrL).

tb_explore_state(St,D,N,StLangL,DoneL,[trans(St,X,ToSt)|TrL],[[ToSt,DD]|NewToDoL],NewDoneL2,[trans(St,X,ToSt)|NewTrl],UnusedTrL) :-
  !,
  DD is D+1,
  append(DoneL,[ToSt],NewDoneL1),
  tb_explore_state(St,D,N,StLangL,NewDoneL1,TrL,NewToDoL,NewDoneL2,NewTrl,UnusedTrL).

tb_explore_state(St,D,N,StLangL,DoneL,[Tr|TrL],NewToDoL,NewDoneL,NewTrl,[Tr|UnusedTrL]) :-
  !,
  tb_explore_state(St,D,N,StLangL,DoneL,TrL,NewToDoL,NewDoneL,NewTrl,UnusedTrL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb_explore_automaton([],_,_,_,_,[]) :- !.

tb_explore_automaton([[St,D]|ToDoL],N,StLangL,DoneL,TrL,NewTrl3) :-
  tb_explore_state(St,D,N,StLangL,DoneL,TrL,NewToDoL1,NewDoneL,NewTrl1,UnusedTrL),
  append(ToDoL,NewToDoL1,NewToDoL2),
  tb_explore_automaton(NewToDoL2,N,StLangL,NewDoneL,UnusedTrL,NewTrl2),
  append(NewTrl1,NewTrl2,NewTrl3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% abs_tracht_barzd(fa(Pred,NQ,[0],FinL,TrL,JL),NewAut) :-
% !,
% write('-- Trachtenbrot-Barzdin based on extracting languages of states in action ...'), nl,
% gen_conf_up_to(N),
% tb_lang_of_states(0,fa(Pred,NQ,[0],FinL,TrL,JL),StLangL),
% tb_explore_automaton([[0,0]],N,StLangL,[0],TrL,NewTrl),
% fsa_regex_compile(mb(fa(fa(Pred,NQ,[0],FinL,NewTrl,JL))),NewAut),
% NewAut=fa(_,NNQ,_,_,_,_), write('-- TB: '), write(NQ), write(' --> '), write(NNQ), nl,
% !.

% abs_tracht_barzd(_,_) :-
% write('abs_tracht_barzd: More than one initial state - exiting...'), nl,
% halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trachtenbrot - Barzdin (via modified minimization computation)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We compute a list of partitions containing a list of classes for each max
% distance to the final states. (The computation stops because TB deals with
% automata of bounded-length words.)

tb2_do_fl_comp_classes(N,QN,ReachQUpToNL,SigL,TrL,ClUpToNL,[[N,ClUpToNL]|ClLL]) :-
  NN is N+1,
  fl_comp_new_classes(QN,ReachQUpToNL,SigL,TrL,ClUpToNL,ReachQUpToNNL,ClUpToNNL),
  ( (ReachQUpToNL=ReachQUpToNNL,ClUpToNL=ClUpToNNL, !, ClLL=[]);
    (tb2_do_fl_comp_classes(NN,QN,ReachQUpToNNL,SigL,TrL,ClUpToNNL,ClLL)) ),
  !.

%-------------------------------------------------------------------------------

tb2_fl_comp_classes(QN,FL,TrL,ClLL) :-
  sigma(SigL),
  fl_comp_nonf(0,QN,FL,NonFL),
  tb2_do_fl_comp_classes(0,QN,FL,SigL,TrL,[[0,FL],[1,NonFL]],ClLL),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb2_ft_comp_classes(QN,TrL,ClLL) :-
  sigma(SigL),
  ft_gen_st_list(QN,QL),
  tb2_do_fl_comp_classes(0,QN,QL,SigL,TrL,[[0,QL]],ClLL),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb2_do_fb_comp_classes(N,QN,ReachQUpToNL,SigL,TrL,ClUpToNL,[[N,ClUpToNL]|ClLL]) :-
  NN is N+1,
  fb_comp_new_classes(QN,ReachQUpToNL,SigL,TrL,ClUpToNL,ReachQUpToNNL,ClUpToNNL),
  ( (ReachQUpToNL=ReachQUpToNNL,ClUpToNL=ClUpToNNL, !, ClLL=[]);
    (tb2_do_fb_comp_classes(NN,QN,ReachQUpToNNL,SigL,TrL,ClUpToNNL,ClLL)) ),
  !.

%-------------------------------------------------------------------------------

tb2_fb_comp_classes(QN,IL,TrL,ClLL) :-
  sigma(SigL),
  fl_comp_nonf(0,QN,IL,NonIL),
  tb2_do_fb_comp_classes(0,QN,IL,SigL,TrL,[[0,IL],[1,NonIL]],ClLL),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb2_fbt_comp_classes(QN,TrL,ClLL) :-
  sigma(SigL),
  ft_gen_st_list(QN,QL),
  tb2_do_fb_comp_classes(0,QN,QL,SigL,TrL,[[0,QL]],ClLL),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb2_class_at_level(State,N,ClLL,[N,Cl]) :-
  member([N,ClL],ClLL),
  member([Cl,StL],ClL),
  member(State,StL),
  !.

tb2_class_at_level(State,N,ClLL,StLevCl) :-
  NN is N-1,
  tb2_class_at_level(State,NN,ClLL,StLevCl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb2_do_try_to_merge_state(StLevCl,N,ClLL,[RedirState|_],RedirState) :-
  tb2_class_at_level(RedirState,N,ClLL,RedirStLevCl),
  StLevCl=RedirStLevCl, %%% Must be like this NOT to make tb2_class_at_level loop forever!!!
  !.

tb2_do_try_to_merge_state(StLevCl,N,ClLL,[_|RedirStateL],RedirState) :-
  tb2_do_try_to_merge_state(StLevCl,N,ClLL,RedirStateL,RedirState).

%-------------------------------------------------------------------------------

tb2_try_to_merge_state(State,Depth,N,ClLL,DoneL,RedirState) :-
  NN is N-Depth,
  tb2_class_at_level(State,NN,ClLL,StLevCl),
  tb2_do_try_to_merge_state(StLevCl,NN,ClLL,DoneL,RedirState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb2_explore_state(_,_,_,_,DoneL,[],[],DoneL,[],[]) :- !.

tb2_explore_state(St,D,N,ClLL,DoneL,[trans(St,X,ToSt)|TrL],NewToDoL,NewDoneL,[trans(St,X,RedirSt)|NewTrl],UnusedTrL) :-
  DD is D+1,
  tb2_try_to_merge_state(ToSt,DD,N,ClLL,DoneL,RedirSt),
  !,
  tb2_explore_state(St,D,N,ClLL,DoneL,TrL,NewToDoL,NewDoneL,NewTrl,UnusedTrL).

tb2_explore_state(St,D,N,ClLL,DoneL,[trans(St,X,ToSt)|TrL],[[ToSt,DD]|NewToDoL],NewDoneL2,[trans(St,X,ToSt)|NewTrl],UnusedTrL) :-
  !,
  DD is D+1,
  append(DoneL,[ToSt],NewDoneL1),
  tb2_explore_state(St,D,N,ClLL,NewDoneL1,TrL,NewToDoL,NewDoneL2,NewTrl,UnusedTrL).

tb2_explore_state(St,D,N,ClLL,DoneL,[Tr|TrL],NewToDoL,NewDoneL,NewTrl,[Tr|UnusedTrL]) :-
  !,
  tb2_explore_state(St,D,N,ClLL,DoneL,TrL,NewToDoL,NewDoneL,NewTrl,UnusedTrL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tb2_explore_automaton([],_,_,_,_,[]) :- !.

tb2_explore_automaton([[St,D]|ToDoL],N,ClLL,DoneL,TrL,NewTrl3) :-
  tb2_explore_state(St,D,N,ClLL,DoneL,TrL,NewToDoL1,NewDoneL,NewTrl1,UnusedTrL),
  append(ToDoL,NewToDoL1,NewToDoL2),
  tb2_explore_automaton(NewToDoL2,N,ClLL,NewDoneL,UnusedTrL,NewTrl2),
  append(NewTrl1,NewTrl2,NewTrl3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abs_tracht_barzd(fa(Pred,NQ,[0],FinL,TrL,JL),NewAut) :-
  !,
  write('-- Trachtenbrot-Barzdin based on modified minimization in action ...'), nl,
  gen_conf_up_to(N),
  tb2_explore_automaton([[0,0]],N,ClLL,[0],TrL,NewTrl),
  %% fsa_regex_compile(mb(fa(fa(Pred,NQ,[0],FinL,NewTrl,JL))),NewAut), %% Choose this or vvv
  NewAut=fa(Pred,NQ,[0],FinL,NewTrl,JL), %% Choose this or ^^^
  NewAut=fa(_,NNQ,_,_,_,_), write('-- TB: '), write(NQ), write(' --> '), write(NNQ), nl,
  !.

abs_tracht_barzd(_,_) :-
  write('abs_tracht_barzd: More than one initial state - exiting...'), nl,
  halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstraction by collapsing wrt. finite length forward languages refined
% by traces up to the same length. (For a similar version see: abstr_fl_refby_ft).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstr_fl_ft(Conc,Abstr) :-
  Conc=fa(ConcSym,ConcQN,ConcIniL,ConcFinL,ConcTrL,_),
  fin_lang_up_to(Lim),
  sigma(SigL),
  %%% write('- Computing the partition wrt. languages up to '),write(Lim),write(' letters...'),nl,
  fl_comp_nonf(0,ConcQN,ConcFinL,ConcNonFL),
  fl_comp_classes(0,Lim,ConcQN,ConcFinL,SigL,ConcTrL,[[0,ConcFinL],[1,ConcNonFL]],Cl1L),
  %%% write('Partition 1: '),maplist(fl_take_snd,Cl1L,Partition1L),write(Partition1L),nl,
  write('- Computing the partition wrt. traces up to '),write(Lim),write(' letters...'),nl,
  ft_gen_st_list(ConcQN,QL),
  fl_comp_classes(0,Lim,ConcQN,QL,SigL,ConcTrL,Cl1L,Cl2L),
  maplist(fl_take_snd,Cl2L,Partition2L),
  %%% write('Partition 2: '),write(Partition2L),nl,
  adjust_trans(Partition2L,ConcTrL,NewConcTrL),
  adjust_st(Partition2L,ConcFinL,NewConcFinL),
  adjust_st(Partition2L,ConcIniL,NewConcIniL),
  fsa_construct_rename_states(ConcSym,NewConcIniL,NewConcFinL,NewConcTrL,[],Abstr1),
  fsa_regex_compile(mb(fa(Abstr1)),Abstr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Acceleration via detecting unwound loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Detecting an unwound loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% We expect deterministic automata !!!

successor_via_letter(Q1,A,[trans(Q1,B,Q2)|_],Q2) :- fsa_preds:conjunction(A,B,_), !.

successor_via_letter(Q1,A,[_|TrL],Q2) :- successor_via_letter(Q1,A,TrL,Q2).

%-------------------------------------------------------------------------------

%%% We consider non-looping paths only and containing self-loops on any state.

successor_via_ntimes_a_letter(0,Q,A,TrL,Visited,Visited) :-
  !.

successor_via_ntimes_a_letter(N,Q1,A,TrL,Visited,AllVisited) :-
  successor_via_letter(Q1,A,TrL,Q2),
  not(member(Q2,Visited)),
  NN is N-1,
  successor_via_ntimes_a_letter(NN,Q2,A,TrL,[Q2|Visited],AllVisited).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

successor_via_word(Q,[],_,Q) :- !.

successor_via_word(Q1,[A|AL],TrL,Q3) :-
  successor_via_letter(Q1,A,TrL,Q2),
  successor_via_word(Q2,AL,TrL,Q3).

%-------------------------------------------------------------------------------

%%% We consider non-looping paths only.

successor_via_ntimes_a_word(0,Q,_,_,Visited,Visited) :- !.

successor_via_ntimes_a_word(N,Q1,AL,TrL,Visited,AllVisited) :-
  successor_via_word(Q1,AL,TrL,Q2),
  not(member(Q2,Visited)),
  NN is N-1,
  successor_via_ntimes_a_word(NN,Q2,AL,TrL,[Q2|Visited],AllVisited).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main loop of accelerating via unwound self-loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main loop of accelerating via unwound self-loops
% !!! Adding a special state with the new self-loop !!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



add_out_tr_after_new_self_loop(_,_,[],[]) :- !.

add_out_tr_after_new_self_loop(NewSt,OldSt,[trans(OldSt,Via,ToSt)|TrL],VeryNewTrL) :-
  add_out_tr_after_new_self_loop(NewSt,OldSt,TrL,NewTrL),
  VeryNewTrL=[trans(NewSt,Via,ToSt),trans(OldSt,Via,ToSt)|NewTrL],
  !.

add_out_tr_after_new_self_loop(NewSt,OldSt,[trans(FromSt,Via,ToSt)|TrL],VeryNewTrL) :-
  add_out_tr_after_new_self_loop(NewSt,OldSt,TrL,NewTrL),
  VeryNewTrL=[trans(FromSt,Via,ToSt)|NewTrL].

%-------------------------------------------------------------------------------

do_accel_unwound_self_loops(St,St,_,_,TrL,St,TrL,0) :- !.

do_accel_unwound_self_loops(St,StN,UnwoundTimes,/,TrL,VeryNewStN,VeryNewTrL,1) :-
  successor_via_ntimes_a_letter(UnwoundTimes,St,/,TrL,[St],[C,B,A|_]),
  not(member(trans(C,/,C),TrL)),
  NextSt is St+1,
  do_accel_unwound_self_loops(NextSt,StN,UnwoundTimes,/,TrL,NewStN,NewTrL,_),
  write('!!! Adding a self-loop from:'),write(A),write('-'),write(B),write(-),write(C),nl,
  VeryNewStN is NewStN+1,
  %% add_out_tr_after_new_self_loop(NewStN,B,NewTrL,AddedOutTrL), %% Choose this or vvv ...
  %% VeryNewTrL=[trans(A,/,NewStN),trans(NewStN,/,NewStN)|AddedOutTrL],
  add_out_tr_after_new_self_loop(NewStN,C,NewTrL,AddedOutTrL), %% Choose this or ^^^ ...
    VeryNewTrL=[trans(A,/,NewStN),trans(NewStN,/,NewStN)|AddedOutTrL],
  !.

do_accel_unwound_self_loops(St,StN,UnwoundTimes,/,TrL,NewStN,NewTrL,AccDone) :-
  NextSt is St+1,
  do_accel_unwound_self_loops(NextSt,StN,UnwoundTimes,/,TrL,NewStN,NewTrL,AccDone).

%-------------------------------------------------------------------------------

accel_unwound_self_loops(/,States,AccelStates) :-
  States=fa(PredModule,StN,IniL,FinL,TrL,[]),
  unwound_times(UnwoundTimes),
  write('Accelerating via '),write(/),write('-self loops unwound '),
    write(UnwoundTimes),write(' times and added to new states...'),nl,
  do_accel_unwound_self_loops(0,StN,UnwoundTimes,/,TrL,NewStN,NewTrL,AccDone),
  ( (AccDone=0, !, AccelStates=States);
    (fsa_regex_compile(mb(cleanup(fa(fa(PredModule,NewStN,IniL,FinL,NewTrL,[])))),AccelStates)) ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main loop of accelerating via unwound general loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_add_accel_unwound_loop(FromTo,From,[A],[trans(From,A,FromTo)]) :- !.

do_add_accel_unwound_loop(FromTo,From,[A|AL],[trans(From,A,To)|NewTrL]) :-
  To is From+1,
  do_add_accel_unwound_loop(FromTo,To,AL,NewTrL).

%-------------------------------------------------------------------------------

add_accel_unwound_loop(TotStNum,FromTo,[A|AL],[trans(FromTo,A,TotStNum)|NewTrL]) :-
  do_add_accel_unwound_loop(FromTo,TotStNum,AL,NewTrL).

%-------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main loop of accelerating via unwound general loops
% !!! Adding a special state with the new self-loop !!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



add_input_to_new_loop(From,To,Final,[A],[trans(From,A,Final)]) :- !.

add_input_to_new_loop(From,To,Final,[A|L],[trans(From,A,To)|NewTrL]) :-
  NewTo is To+1,
  add_input_to_new_loop(To,NewTo,Final,L,NewTrL).

%-------------------------------------------------------------------------------

do_accel_unwound_loops(StN,StN,_,_,_,TrL,StN,TrL,0) :- !.

do_accel_unwound_loops(St,StN,UnwoundTimes,LoopL,LoopLen,TrL,VeryNewStN,VeryNewTrL,1) :-
  successor_via_ntimes_a_word(UnwoundTimes,St,LoopL,TrL,[St],[C,B,A|_]),
  not(successor_via_a_word(C,LoopL,TrL,C)),
  NextSt is St+1,
  do_accel_unwound_loops(NextSt,StN,UnwoundTimes,LoopL,LoopLen,TrL,NewStN1,NewTrL1,_),
  write('!!! Adding a loop from:'),write(A),write('-'),write(B),write(-),write(C),nl,
  NewStN2 is NewStN1+1,
  add_input_to_new_loop(A,NewStN2,NewStN1,LoopL,NewTrL2),
  NewStN3 is NewStN1+LoopLen,
  add_accel_unwound_loop(NewStN3,NewStN1,LoopL,NewTrL3),
  VeryNewStN is NewStN3+LoopLen-1,
  %% add_out_tr_after_new_self_loop(NewStN1,B,NewTrL1,NewTrL4), %% Choose this or vvv ...
  add_out_tr_after_new_self_loop(NewStN1,C,NewTrL1,NewTrL4), %% Choose this or ^^^ ...
  append(NewTrL4,NewTrL2,NewTrL5),
  append(NewTrL5,NewTrL3,VeryNewTrL),
  !.

do_accel_unwound_loops(St,StN,UnwoundTimes,LoopL,LoopLen,TrL,NewStN,NewTrL,AccDone) :-
  NextSt is St+1,
  do_accel_unwound_loops(NextSt,StN,UnwoundTimes,LoopL,LoopLen,TrL,NewStN,NewTrL,AccDone).

%-------------------------------------------------------------------------------

accel_unwound_loops(LoopL,States,AccelStates) :-
  States=fa(PredModule,StN,IniL,FinL,TrL,[]),
  length(LoopL,LoopLen),
  unwound_times(UnwoundTimes),
  write('Accelerating via '),write(LoopL),write(' loops unwound '),
    write(UnwoundTimes),write(' times and added to new states...'),nl,
  do_accel_unwound_loops(0,StN,UnwoundTimes,LoopL,LoopLen,TrL,NewStN,NewTrL,AccDone),
  ( (AccDone=0, !, AccelStates=States);
    (fsa_regex_compile(mb(cleanup(fa(fa(PredModule,NewStN,IniL,FinL,NewTrL,[])))),AccelStates)) ).
    %% (fsa_regex_compile(fa(fa(PredModule,NewStN,IniL,FinL,NewTrL,[])),AccelStates)) ). %%% When using a separate union.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Acceleration via detecting several unwound loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_accel_several_unwound_loops([],States,States) :- !.

do_accel_several_unwound_loops([LoopL|LoopsRestL],States,AccelStates) :-
  States=fa(PredModule,StN,IniL,FinL,TrL,[]),
  length(LoopL,LoopLen),
  unwound_times(UnwoundTimes),
  write('Accelerating via '),write(LoopL),write(' loops unwound '),
    write(UnwoundTimes),write(' times...'),nl,



    do_accel_unwound_loops(0,StN,UnwoundTimes,LoopL,LoopLen,TrL,NewStN,NewTrL,_),

  do_accel_several_unwound_loops(LoopsRestL,fa(PredModule,NewStN,IniL,FinL,NewTrL,[]),AccelStates).

%-------------------------------------------------------------------------------

accel_several_unwound_loops(LoopsL,States,MinAccelStates) :-
  do_accel_several_unwound_loops(LoopsL,States,AccelStates),
  fsa_regex_compile(mb(cleanup(fa(AccelStates))),MinAccelStates).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing classes wrt. reachability via repeatable symbols (e.g., in lists).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_all_from([],_,[]).

delete_all_from([X|L1],L2,L3) :-
  member(X,L2),
  !,
  delete_all_from(L1,L2,L3).

delete_all_from([X|L1],L2,[X|L3]) :-
  delete_all_from(L1,L2,L3).

%-------------------------------------------------------------------------------

fw_bw_one_reach_via_rep_sym(From,[trans(From,X,To)|TrL1],StL,RepSymL,ReachL1,ToDoL1,ReachL3,ToDoL3,TrL2) :-
  member(To,StL),
  fsa_preds:conjunction(X,in(RepSymL),_),
  ( (member(To,ReachL1), !, ReachL2=ReachL1, ToDoL2=ToDoL1);
    (ReachL2=[To|ReachL1], ToDoL2=[To|ToDoL1]) ),
  fw_bw_one_reach_via_rep_sym(From,TrL1,StL,RepSymL,ReachL2,ToDoL2,ReachL3,ToDoL3,TrL2),
  !.

fw_bw_one_reach_via_rep_sym(From,[trans(To,X,From)|TrL1],StL,RepSymL,ReachL1,ToDoL1,ReachL3,ToDoL3,TrL2) :-
  member(To,StL),
  fsa_preds:conjunction(X,in(RepSymL),_),
  ( (member(To,ReachL1), !, ReachL2=ReachL1, ToDoL2=ToDoL1);
    (ReachL2=[To|ReachL1], ToDoL2=[To|ToDoL1]) ),
  fw_bw_one_reach_via_rep_sym(From,TrL1,StL,RepSymL,ReachL2,ToDoL2,ReachL3,ToDoL3,TrL2),
  !.

fw_bw_one_reach_via_rep_sym(From,[T|TrL1],StL,RepSymL,ReachL1,ToDoL1,ReachL2,ToDoL2,[T|TrL2]) :-
  fw_bw_one_reach_via_rep_sym(From,TrL1,StL,RepSymL,ReachL1,ToDoL1,ReachL2,ToDoL2,TrL2),
  !.

fw_bw_one_reach_via_rep_sym(_,[],StL,_,ReachL,ToDoL,ReachL,ToDoL,[]).

%-------------------------------------------------------------------------------

fw_bw_reach_via_rep_sym([From|ToDoL1],ReachL1,TrL1,StL,RepSymL,ReachL3,TrL3) :-
  fw_bw_one_reach_via_rep_sym(From,TrL1,StL,RepSymL,ReachL1,ToDoL1,ReachL2,ToDoL2,TrL2),
  fw_bw_reach_via_rep_sym(ToDoL2,ReachL2,TrL2,StL,RepSymL,ReachL3,TrL3),
  !.

fw_bw_reach_via_rep_sym([],ReachL,TrL,_,_,ReachL,TrL).

%-------------------------------------------------------------------------------

split_cls_wrt_rch_via_rep_sym(ClN,[St|StL],TrL1,RepSymL,NewClN,[[ClN,ReachL]|RestClL],TrL3) :-
  fw_bw_reach_via_rep_sym([St],[St],TrL1,[St|StL],RepSymL,ReachL,TrL2),
  delete_all_from([St|StL],ReachL,RestStL),
  NextClN is ClN+1,
  split_cls_wrt_rch_via_rep_sym(NextClN,RestStL,TrL2,RepSymL,NewClN,RestClL,TrL3),
  !.

split_cls_wrt_rch_via_rep_sym(ClN,[],TrL,_,ClN,[],TrL).

%-------------------------------------------------------------------------------

do_split_class_list_wrt_rep_sym([[_,[St]]|ClL],TrL1,RepSymL,ClNum1,NewClL,TrL2) :-
  ClNum2 is ClNum1+1,
  do_split_class_list_wrt_rep_sym(ClL,TrL1,RepSymL,ClNum2,NewClL2,TrL2),
  NewClL=[[ClNum1,[St]]|NewClL2],
  !.

do_split_class_list_wrt_rep_sym([[_,Cl]|ClL],TrL1,RepSymL,ClNum1,NewClL,TrL3) :-
  split_cls_wrt_rch_via_rep_sym(ClNum1,Cl,TrL1,RepSymL,ClNum2,NewClL1,TrL2),
  do_split_class_list_wrt_rep_sym(ClL,TrL2,RepSymL,ClNum2,NewClL2,TrL3),
  append(NewClL1,NewClL2,NewClL),
  !.

do_split_class_list_wrt_rep_sym([],TrL,_,_,[],TrL).

%...............................................................................

split_class_list_wrt_rep_sym(ClL,TrL,NewClL) :-
  rep_sym(RepSymL),
  write('Refining wrt. reachability via repeatable symbols: '),write(RepSymL),nl,
  do_split_class_list_wrt_rep_sym(ClL,TrL,RepSymL,0,NewClL,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing classes wrt. reachability via repeatable symbols (e.g., in lists).
% ( A version in which we do not number the classes...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_cls_wrt_rch_via_rep_sym_unnum([St|StL],TrL1,RepSymL,[ReachL|RestClL],TrL3) :-
  fw_bw_reach_via_rep_sym([St],[St],TrL1,[St|StL],RepSymL,ReachL,TrL2),
  delete_all_from([St|StL],ReachL,RestStL),
  split_cls_wrt_rch_via_rep_sym_unnum(RestStL,TrL2,RepSymL,RestClL,TrL3),
  !.

split_cls_wrt_rch_via_rep_sym_unnum([],TrL,_,[],TrL).

%-------------------------------------------------------------------------------

do_split_class_list_wrt_rep_sym_unnum([[St]|ClL],TrL1,RepSymL,ClNum1,NewClL,TrL2) :-
  do_split_class_list_wrt_rep_sym_unnum(ClL,TrL1,RepSymL,NewClL2,TrL2),
  NewClL=[[St]|NewClL2],
  !.

do_split_class_list_wrt_rep_sym_unnum([Cl|ClL],TrL1,RepSymL,NewClL,TrL3) :-
  split_cls_wrt_rch_via_rep_sym_unnum(Cl,TrL1,RepSymL,NewClL1,TrL2),
  do_split_class_list_wrt_rep_sym_unnum(ClL,TrL2,RepSymL,NewClL2,TrL3),
  append(NewClL1,NewClL2,NewClL),
  !.

do_split_class_list_wrt_rep_sym_unnum([],TrL,_,[],TrL).

%...............................................................................

split_class_list_wrt_rep_sym_unnum(ClL,TrL,NewClL) :-
  rep_sym(RepSymL),
  write('Refining wrt. reachability via repeatable symbols: '),write(RepSymL),nl,
  do_split_class_list_wrt_rep_sym_unnum(ClL,TrL,RepSymL,NewClL,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main loop for non-united transducers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% One run with a non-united transducer:

septr_do_one_run(N,Line,St,TrL,ReachedL,Result,Value) :-
  member([Line,LineTrL],TrL),
  septr_do_one_run_ph0(N,Line,St,LineTrL,TrL,ReachedL,Result,Value).

%...............................................................................

%% Choose the next transition to fire.

septr_do_one_run_ph0(N,Line,FromSt,LineTrL,TrL,ReachedL,Result2,Value2) :-
  %% write('... phase 0'),nl,
  ( (LineTrL=[[ToLine,Tr,ITr]|LineTrL2],
     !,
     NN is N+1,
     write('... from line '),write(Line),write(' to line '),write(ToLine),nl,
     septr_do_one_run_ph1(NN,FromSt,ToLine,Tr,ITr,TrL,ReachedL,Result,Value),
     ( (Result=good,
        !,
        septr_do_one_run_ph0(N,Line,FromSt,LineTrL2,TrL,Value,Result2,Value2));
       (Result2=Result,Value2=Value) ) );
    (Result2=good,Value2=ReachedL) ).

%...............................................................................

%% Firing one transition.

septr_do_one_run_ph1(N,FromSt,ToLine,Tr,ITr,TrL,ReachedL,Result,Value) :-
  %% write('... phase 1'),nl,
  write('Computing post...'),nl,
  FromSt=fa(_,NS1,_,_,T1,_),length(T1,NT1),
  write('Source automaton: '),write(NS1),write(','),write(NT1),nl,
  fsa_regex_compile(mb(range(compose(fa(FromSt),fa(Tr)))),ToSt),
  ToSt=fa(_,NS2,_,_,T2,_),length(T2,NT2),
  write('Target automaton: '),write(NS2),write(','),write(NT2),nl,
  ( (ToSt=fa(r(fsa_preds),1,[0],[],[],[]), !, Result=good,Value=ReachedL); %% No (i.e. empty) successors.
    (sum_bad(Bad),
     write('Intersecting with bad states...'),nl,
     fsa_regex_compile(intersect(fa(ToSt),fa(Bad)),ImmReachBad),



     ( (empty(ImmReachBad), !,
        septr_do_one_run_ph2(N,FromSt,ToLine,ToSt,Tr,ITr,TrL,ReachedL,Result,Value));
       (write('XXXXXXXXX'),nl,
        septr_do_one_run_ph2b(N,FromSt,ToLine,ToSt,Tr,ITr,TrL,ReachedL,ImmReachBad,Result,Value)) )) ).

%...............................................................................

%% Check the fixpoint property.

septr_do_one_run_ph2(N,FromSt,ToLine,ToSt,Tr,ITr,TrL,ReachedL,Result,Value) :-
  %% write('... phase 2'),nl,
  member([ToLine,ReachAtLine],ReachedL),
  ( %% (ReachAtLine=ToSt, !,Result=good,Value=ReachedL); %% Choose this or vvv
    (is_included(ToSt,ReachAtLine), !,Result=good,Value=ReachedL); %% choose this or ^^^
    (write('>>> '),write(N),write(' >>>'),nl,
     %% name(N,NL),atom_chars(PN,NL),atom_concat('step',PN,PP),fsa_write_file(PP,ToSt),
     septr_do_one_run_ph3(N,FromSt,ToLine,ToSt,Tr,ITr,TrL,ReachedL,Result,Value)) ).

%...............................................................................

%% Abstraction after a step forward.

septr_do_one_run_ph3(N,FromSt,ToLine,ToSt,Tr,ITr,TrL,ReachedL,Result,Value) :-
  %% write('... phase 3'),nl,

   abstract(ToSt,ToAbSt),




  sum_bad(Bad),
  write('Intersecting the abstraction with bad states...'),nl,
  fsa_regex_compile(intersect(fa(ToAbSt),fa(Bad)),AbsImmBad),



  ( (empty(AbsImmBad), !,
     septr_do_one_run_ph4(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,Result,Value));
    (write('YYYYYYYYY'),nl,
     septr_do_one_run_ph5(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,[AbsImmBad,AbsImmBad],Result,Value)) ).

%...............................................................................

%% Continue the DFS.

augment_reachable(Line,[[Line,ReachAtLine]|ReachedL],NewSt,[[Line,ReachAtLine2]|ReachedL]) :-
  fsa_regex_compile(mb(union(fa(ReachAtLine),fa(NewSt))),ReachAtLine2),
  !.

augment_reachable(Line,[X|ReachedL],NewSt,[X|ReachedL2]) :-
  augment_reachable(Line,ReachedL,NewSt,ReachedL2),
  !.

septr_do_one_run_ph4(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,Result2,Value2) :-
  %% write('... phase 4'),nl,
  write('Adding the newly reached states...'),nl,
  augment_reachable(ToLine,ReachedL,ToAbSt,ReachedL2),
  septr_do_one_run(N,ToLine,ToAbSt,TrL,ReachedL2,Result,Value),
  ( (Result=good, !, Result2=Result,Value2=Value);
    (Result=new, !, Result2=Result,Value2=Value);
    (septr_do_one_run_ph5(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,Value,Result2,Value2)) ).

%...............................................................................

%% A step back.

septr_do_one_run_ph5(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,[ToErr,ToBackErr],Result,Value) :-
  %% write('... phase 5'),nl,
  write('Computing pre...'),nl,
  fsa_regex_compile(mb(range(compose(fa(ToErr),fa(ITr)))),BackErr),
  BackErr=fa(_,NS1,_,_,T1,_),length(T1,NT1),
  write('After a step back: '),write(NS1),write(','),write(NT1),nl,
  write('Intersecting with reachable...'),nl,
  fsa_regex_compile(intersect(fa(BackErr),fa(FromSt)),FromErr),
  FromErr=fa(_,NS2,_,_,T2,_),length(T2,NT2),
  write('Reachable bad states: '),write(NS2),write(','),write(NT2),nl,
  ( (empty(FromErr), !,
     septr_do_one_run_ph6(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,ToBackErr,Result,Value)); %% A big or small
     %% septr_do_one_run_ph6(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,ToErr,Result,Value)); %% new pred (vvv too)
    (augment_summary_bad(BackErr),
     Result=bad,Value=[FromErr,BackErr],write('<<<<<'),nl) ).

%...............................................................................

%% We go on an incremental run OR after a refinement, we restart from the very beginning.
septr_do_one_run_ph6(N,FromSt,ToLine,ToSt,ToAbSt,Tr,ITr,TrL,ReachedL,Err,Result,Value) :-
  %% write('... phase 6'),nl,
  Result=new,Value=[ToAbSt,Err],
  !.



%...............................................................................

septr_do_one_run_ph2b(N,FromSt,ToLine,ToSt,Tr,ITr,TrL,ReachedL,ToErr,Result,Value) :-
  %% write('... phase 2b'),nl,
  write('Computing pre...'),nl,
  fsa_regex_compile(mb(range(compose(fa(ToErr),fa(ITr)))),BackErr),
  BackErr=fa(_,NS1,_,_,T1,_),length(T1,NT1),
  write('After a step back: '),write(NS1),write(','),write(NT1),nl,
  write('Intersecting with reachable...'),nl,
  fsa_regex_compile(intersect(fa(BackErr),fa(FromSt)),FromErr),
  FromErr=fa(_,NS2,_,_,T2,_),length(T2,NT2),
  write('Reachable bad states: '),write(NS2),write(','),write(NT2),nl,
  augment_summary_bad(BackErr),
  Result=bad,Value=[FromErr,BackErr],write('<<<<<'),nl.

%-------------------------------------------------------------------------------

%%% Result: good---property verified, bad---property broken (for Result1, __may be__ broken),
%%% new---a new predicate has been found
%%% Value: for Result2=good, the abstract reachability set,
%%% for Result2=bad, the error reached

septr_one_run(IniLine,IniSt,TrL,ReachedL,Result2,Value2) :-
  write('Abstracting init...'),nl, abstract(IniSt,IniAbsSt), %% Choose either this or 'v'
  %% IniAbsSt=IniSt, %% Choose either this or '^'
  write('>>> 1 >>>'),nl,
  septr_do_one_run(1,IniLine,IniSt,TrL,ReachedL,Result1,Value1),
  ( (Result1=good, !,Result2=Result1,Value2=Value1);
    (Result1=new, !,Result2=Result1,Value2=Value1);
    (Value1=[FromErr,BackErr],
     write('Intersecting with init...'),nl,
     fsa_regex_compile(intersect(fa(FromErr),fa(IniSt)),IniBadSt),
     IniBadSt=fa(_,NS2,_,_,T2,_),length(T2,NT2),
     write('After an inters. with initial states: '),write(NS2),write(','),write(NT2),nl,
     ( (empty(IniBadSt), !, Result2=new,Value2=[Init,BackErr]); %% A big or small
       %% (empty(IniBadSt), !, Result2=new,Value2=[Init,FromErr]); %% new pred (^^^ too)
       (Result2=bad,Value2=IniBadSt) ) ) ).

%-------------------------------------------------------------------------------

septr_save_reachable([[Line,ReachAtLine]|ReachableL]) :-
  atom_concat('reached_',Line,RL),fsa_write_file(RL,ReachAtLine),
  septr_save_reachable(ReachableL),
  !.

septr_save_reachable([]).

%...............................................................................

septr_prepare_ini_reach([[IniLine,_]|TrL],IniLine,IniSt,Empty,[[IniLine,IniSt]|IniReachL]) :-
  !,
  septr_prepare_ini_reach(TrL,IniLine,IniSt,Empty,IniReachL).

septr_prepare_ini_reach([[Line,_]|TrL],IniLine,IniSt,Empty,[[Line,Empty]|IniReachL]) :-
  !,
  septr_prepare_ini_reach(TrL,IniLine,IniSt,Empty,IniReachL).

septr_prepare_ini_reach([],_,_,_,[]).

%...............................................................................

septr_do_asmc(IniLine,IniSt,Empty,TrL) :-
  retract(run(OldN)),
  N is OldN+1,
  assertz(run(N)),
  septr_prepare_ini_reach(TrL,IniLine,IniSt,Empty,IniReachL),
  write('~~~~~ Run '),write(N),write(':'),nl,
  septr_one_run(IniLine,IniSt,TrL,IniReachL,Result,Value),
  ( (Result=good, !, write('----- Property holds!'),nl,
     septr_save_reachable(Value) );
    (Result=bad, !,fsa_write_file('error_from',Value),write('----- Property broken!'),nl);
    (Value=[CritConcState,NewPred],refine_abstraction(CritConcState,NewPred),septr_do_asmc(IniLine,IniSt,Empty,TrL)) ),
  !.

%-------------------------------------------------------------------------------

septr_do_prepare_itr([[ToLine,Tr]|LineTrL],[[ToLine,Tr,ITr]|LineTrITrL]) :-
  fsa_regex_compile(inverse(fa(Tr)),ITr),
  septr_do_prepare_itr(LineTrL,LineTrITrL),
  !.

septr_do_prepare_itr([],[]).

%...............................................................................

septr_prepare_itr([[Line,LineTrL]|TrL],[[Line,LineTrITrL]|TrITrL]) :-
  septr_do_prepare_itr(LineTrL,LineTrITrL),
  septr_prepare_itr(TrL,TrITrL),
  !.

septr_prepare_itr([],[]).

%-------------------------------------------------------------------------------

septr_asmc :-
  fsa_regex_atom_compile('{}',Empty),
  assertz(empty(Empty)),
  init(IniLine,IniSt),
  tr(TrL),
  septr_prepare_itr(TrL,TrITrL),
  good(Good),
  fsa_regex_compile(complement(fa(Good)),Bad),
  assertz(bad(Bad)),
  prepare_summary_bad,
  assertz(run(0)),
  septr_do_asmc(IniLine,IniSt,Empty,TrITrL).

%...............................................................................

septr_asmc_bad :-
  fsa_regex_atom_compile('{}',Empty),
  assertz(empty(Empty)),
  init(IniLine,IniSt),
  tr(TrL),
  septr_prepare_itr(TrL,TrITrL),
  prepare_summary_bad,
  assertz(run(0)),
  septr_do_asmc(IniLine,IniSt,Empty,TrITrL).

%...............................................................................

septr_asmc_rev :-
  fsa_regex_atom_compile('{}',Empty),
  assertz(empty(Empty)),
  init(EndLine,BadSt), %%%% There must be a single "bad" line!!!
  itr(TrL),
  septr_prepare_itr(TrL,TrITrL),
  prepare_summary_bad,
  assertz(run(0)),
  septr_do_asmc(EndLine,BadSt,Empty,TrITrL).

%-------------------------------------------------------------------------------

septr_asmc_time_yap :-
  statistics(cputime,[T1,_]),
  septr_asmc,
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

%...............................................................................

septr_asmc_bad_time_yap :-
  statistics(cputime,[T1,_]),
  septr_asmc_bad,
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

%...............................................................................

septr_asmc_rev_time_yap :-
  statistics(cputime,[T1,_]),
  septr_asmc_rev,
  statistics(cputime,[T2,_]),
  T is (T2-T1)/1000,
  write('Cputime used: '),write(T),write(' sec.'),
  name(Bell,[7]),write(Bell),
  nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Acceleration via all possible loops of a given length from given symbols
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_pos_loops_gen_loop(_,[],_,_,_,[]) :- !.

all_pos_loops_gen_loop(0,_,_,_,LoopL,[LoopL]) :- !.

all_pos_loops_gen_loop(N,[DtSym|DtSymL],AllDtSymL,SepSym,LoopL,LoopsL3) :-
  N>0,
  NN is N-1,
  all_pos_loops_gen_loop(NN,AllDtSymL,AllDtSymL,SepSym,[DtSym,SepSym|LoopL],LoopsL1),
  all_pos_loops_gen_loop(N,DtSymL,AllDtSymL,SepSym,LoopL,LoopsL2),
  append(LoopsL1,LoopsL2,LoopsL3),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Acceleration via automatically detected loops of a certain length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Automatically detecting and adding loops of a certain length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aut_loop_det(DtSymL,SepSym,States,AccelStates) :-
  unwound_times(2),
  States=fa(PredModule,StN,IniL,FinL,TrL,[]),
  aut_loop_det_max_seq(StN,TrL,DtSymL,SepSym,MaxCollSeqLen),
  MaxLoopLen is MaxCollSeqLen//2,
  ( (MaxLoopLen>0,
     !,
     do_aut_loop_det(1,MaxLoopLen,2,DtSymL,SepSym,StN,TrL,StN,NewStN,NewTrL,0,AccDone),
     ( (AccDone=0, !, AccelStates=States);
       (fsa_regex_compile(mb(cleanup(fa(fa(PredModule,NewStN,IniL,FinL,NewTrL,[])))),AccelStates)) ));
    (AccelStates=States) ),
  !.

%-------------------------------------------------------------------------------

do_aut_loop_det(LoopLen1,MaxLoopLen,2,DtSymL,SepSym,StN,TrL,NewStN1,NewStN3,NewTrL3,Acc1,Acc3) :-
  LoopLen1 =< MaxLoopLen,
  !,
  write('Autodetecting loops of length '),write(LoopLen1),
    write(' repeated '),write(2),write(' times. '),nl,

      write('  Adding the loops at a new state...'),nl,

  aut_loop_det_iter_states(0,StN,LoopLen1,TrL,DtSymL,SepSym,NewStN1,NewStN2,NewTrL1,Acc1,Acc2),
  LoopLen2 is LoopLen1+1,
  do_aut_loop_det(LoopLen2,MaxLoopLen,2,DtSymL,SepSym,StN,TrL,NewStN2,NewStN3,NewTrL2,Acc2,Acc3),
  append(NewTrL1,NewTrL2,NewTrL3),
  !.

do_aut_loop_det(_,_,_,_,_,_,_,NewStN,NewStN,[],Acc,Acc).

%-------------------------------------------------------------------------------

aut_loop_det_iter_states(StN,StN,_,TrL,_,_,NewN,NewN,TrL,Acc,Acc) :- !.

aut_loop_det_iter_states(St,StN,N,TrL,DtSymL,SepSym,NewN1,NewN3,NewTrL3,Acc1,Acc3) :-

    aut_loop_det_n_stp_via_dt(N,St,TrL,[St],[],TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL1,Acc1,Acc2),



  NextSt is St + 1,
  aut_loop_det_iter_states(NextSt,StN,N,TrL,DtSymL,SepSym,NewN2,NewN3,NewTrL2,Acc2,Acc3),
  append(NewTrL1,NewTrL2,NewTrL3),
  !.

%-------------------------------------------------------------------------------

aut_loop_det_n_stp_via_dt(N1,St1,TrToDoL1,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN1,NewN3,NewTrL3,Acc1,Acc3) :-
  N1>0,
  TrToDoL1=[trans(St1,A,St2)|TrToDoL2],
  not(member(St2,StViaL1)),
  fsa_preds:conjunction(A,in(DtSymL),_),
  N2 is N1-1,
  aut_loop_det_n_stp_via_sep(N2,St2,TrL,[St2|StViaL1],[A|TrViaL1],TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL1,Acc1,Acc2),
  aut_loop_det_n_stp_via_dt(N1,St1,TrToDoL2,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN2,NewN3,NewTrL2,Acc2,Acc3),
  append(NewTrL1,NewTrL2,NewTrL3),
  !.



aut_loop_det_n_stp_via_dt(0,St1,_,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL,Acc1,Acc2) :-
  reverse(TrViaL1,TrViaL2),
  unwound_times(2),
  UnwTimesMinusOne is 2 -1,
  aut_loop_det_snd_n_stp(UnwTimesMinusOne,St1,TrL,StViaL1,TrViaL2,TrL,St,St1,[],NewN1,NewN2,NewTrL,Acc1,Acc2),
  !.



aut_loop_det_n_stp_via_dt(_,_,[],_,_,_,_,_,_,NewN,NewN,[],Acc,Acc) :- !.

aut_loop_det_n_stp_via_dt(N1,St1,TrToDoL1,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL,Acc1,Acc2) :-
  TrToDoL1=[_|TrToDoL2],
  aut_loop_det_n_stp_via_dt(N1,St1,TrToDoL2,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL,Acc1,Acc2),
  !.

%-------------------------------------------------------------------------------

aut_loop_det_n_stp_via_sep(N1,St1,TrToDoL1,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN1,NewN3,NewTrL3,Acc1,Acc3) :-
  TrToDoL1=[trans(St1,A,St2)|TrToDoL2],
  not(member(St2,StViaL1)),
  fsa_preds:conjunction(A,SepSym,_),
  aut_loop_det_n_stp_via_dt(N1,St2,TrL,[St2|StViaL1],[A|TrViaL1],TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL1,Acc1,Acc2),
  aut_loop_det_n_stp_via_sep(N1,St1,TrToDoL2,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN2,NewN3,NewTrL2,Acc2,Acc3),
  append(NewTrL1,NewTrL2,NewTrL3),
  !.
aut_loop_det_n_stp_via_sep(_,_,[],_,_,_,_,_,_,NewN,NewN,[],Acc,Acc) :- !.

aut_loop_det_n_stp_via_sep(N1,St1,TrToDoL1,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL,Acc1,Acc2) :-
  TrToDoL1=[_|TrToDoL2],
  aut_loop_det_n_stp_via_sep(N1,St1,TrToDoL2,StViaL1,TrViaL1,TrL,DtSymL,SepSym,St,NewN1,NewN2,NewTrL,Acc1,Acc2),
  !.

%-------------------------------------------------------------------------------

aut_loop_det_add_out_tr_after_new_self_loop(_,_,[],[]) :- !.

aut_loop_det_add_out_tr_after_new_self_loop(NewSt,OldSt,[trans(OldSt,Via,ToSt)|TrL],VeryNewTrL) :-
  aut_loop_det_add_out_tr_after_new_self_loop(NewSt,OldSt,TrL,NewTrL),
  VeryNewTrL=[trans(NewSt,Via,ToSt)|NewTrL],
  !.

aut_loop_det_add_out_tr_after_new_self_loop(NewSt,OldSt,[_|TrL],NewTrL) :-
  aut_loop_det_add_out_tr_after_new_self_loop(NewSt,OldSt,TrL,NewTrL).

%-------------------------------------------------------------------------------

aut_loop_det_snd_n_stp(N,St1,TrToDoL1,StViaL1,TrViaL1,TrL,StA,StB,RefTrViaL,NewN1,NewN3,NewTrL3,Acc1,Acc3) :-
  TrToDoL1=[trans(St1,A,St2)|TrToDoL2],
  not(member(St2,StViaL1)),
  TrViaL1=[B|TrViaL2],
  fsa_preds:conjunction(A,B,C),
  aut_loop_det_snd_n_stp(N,St2,TrL,[St2|StViaL1],TrViaL2,TrL,StA,StB,[C|RefTrViaL],NewN1,NewN2,NewTrL1,Acc1,Acc2),
  aut_loop_det_snd_n_stp(N,St1,TrToDoL2,StViaL1,TrViaL1,TrL,StA,StB,RefTrViaL,NewN2,NewN3,NewTrL2,Acc2,Acc3),
  append(NewTrL1,NewTrL2,NewTrL3),
  !.
aut_loop_det_snd_n_stp(1,StC,_,_,[],TrL,StA,StB,RefTrViaL1,NewN1,NewN4,NewTrL5,_,1) :-
  reverse(RefTrViaL1,RefTrViaL2),
  write('!!! Adding a '),write(RefTrViaL2),write(' loop from: '),
    write(StA),write('-'),write(StB),write(-),write(StC),nl,
  NewN2 is NewN1 + 1,
  add_input_to_new_loop(StA,NewN2,NewN1,RefTrViaL2,NewTrL1),
  length(RefTrViaL2,LoopLen),
  NewN3 is NewN1 + LoopLen,
  add_accel_unwound_loop(NewN3,NewN1,RefTrViaL2,NewTrL2),
  NewN4 is NewN3 + LoopLen - 1,
  aut_loop_det_add_out_tr_after_new_self_loop(NewN1,StC,TrL,NewTrL3),
  append(NewTrL1,NewTrL2,NewTrL4),
  append(NewTrL4,NewTrL3,NewTrL5),
  !.



aut_loop_det_snd_n_stp(N1,St,_,StViaL,[],TrL,StA,StB,RefTrViaL1,NewN1,NewN2,NewTrL,Acc1,Acc2) :-
  N2 is N1-1,
  reverse(RefTrViaL1,RefTrViaL2),
  aut_loop_det_snd_n_stp(N2,St,TrL,StViaL,RefTrViaL2,TrL,StA,St,[],NewN1,NewN2,NewTrL,Acc1,Acc2),
  !.

aut_loop_det_snd_n_stp(_,_,[],_,_,_,_,_,_,NewN,NewN,[],Acc,Acc) :- !.

aut_loop_det_snd_n_stp(N,St,TrToDoL1,StViaL,TrViaL,TrL,StA,StB,RefTrViaL,NewN1,NewN2,NewTrL,Acc1,Acc2) :-
  TrToDoL1=[_|TrToDoL2],
  aut_loop_det_snd_n_stp(N,St,TrToDoL2,StViaL,TrViaL,TrL,StA,StB,RefTrViaL,NewN1,NewN2,NewTrL,Acc1,Acc2),
  append(NewTrL1,NewTrL2,NewTrL3),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing the length of the maximu collapsable path in an automaton of the
% form [sep,dt]*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aut_loop_det_do_comp_max_seq(Sym1,N1,St1,TrToDoL1,StViaL1,TrL,DtSymL,SepSym,Max) :-
  TrToDoL1=[trans(St1,A,St2)|TrToDoL2],
  not(member(St2,StViaL1)),
  ( (Sym1=dt, !, fsa_preds:conjunction(A,in(DtSymL),_), !, N2 is N1+1,Sym2=sep);
    (fsa_preds:conjunction(A,SepSym,_),N2=N1,Sym2=dt) ),
  aut_loop_det_do_comp_max_seq(Sym2,N2,St2,TrL,[St2|StViaL1],TrL,DtSymL,SepSym,Max1),
  aut_loop_det_do_comp_max_seq(Sym1,N1,St1,TrToDoL2,StViaL1,TrL,DtSymL,SepSym,Max2),
  ( (Max1 > Max2, !, Max=Max1); (Max=Max2) ),
  !.

aut_loop_det_do_comp_max_seq(Sym,N,St,TrToDoL1,StViaL,TrL,DtSymL,SepSym,Max) :-
  TrToDoL1=[_|TrToDoL2],
  aut_loop_det_do_comp_max_seq(Sym,N,St,TrToDoL2,StViaL,TrL,DtSymL,SepSym,Max),
  !.

aut_loop_det_do_comp_max_seq(_,N,_,[],_,_,_,_,N).

%-------------------------------------------------------------------------------

aut_loop_det_comp_max_seq(StN,StN,_,_,_,0) :- !.

aut_loop_det_comp_max_seq(St1,StN,TrL,DtSymL,SepSym,Max) :-
  aut_loop_det_do_comp_max_seq(sep,0,St1,TrL,[St1],TrL,DtSymL,SepSym,Max1),
  St2 is St1+1,
  aut_loop_det_comp_max_seq(St2,StN,TrL,DtSymL,SepSym,Max2),
  ( (Max1 > Max2, !, Max=Max1); (Max=Max2) ),
  !.

%-------------------------------------------------------------------------------

aut_loop_det_max_seq(StN,TrL,DtSymL,SepSym,Max) :-
  aut_loop_det_comp_max_seq(0,StN,TrL,DtSymL,SepSym,Max),
  write('@@@ Current max coll. seq. length: '),write(Max),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% That is all falks...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the abstr_collprice of a slower run...

%% :- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

:- assertz(tr([])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line1_to_line1_e1),L1L1_e1),
   fsa_regex_compile(file(line1_to_line2),L1L2),
   assertz(tr([[l1,[[le1,L1L1_e1],[l2,L1L2]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line2_to_line2_3),L2L2_3),
   assertz(tr([[l2,[[l3,L2L2_3]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line3_to_line3_e2),L3L3_e2),
   fsa_regex_compile(file(line3_to_line4),L3L4),
   assertz(tr([[l3,[[le2,L3L3_e2],[l4,L3L4]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line4_to_line4_5),L4L4_5),
   assertz(tr([[l4,[[l5,L4L4_5]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line5_to_line5),L5L5),
   fsa_regex_compile(file(line5_to_line6),L5L6),
   assertz(tr([[l5,[[l5,L5L5],[l6,L5L6]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line6_to_line6),L6L6),
   fsa_regex_compile(file(line6_to_line7),L6L7),
   assertz(tr([[l6,[[l6,L6L6],[l7,L6L7]]]|TrL])).

:- retract(tr(TrL)),
   assertz(tr([[l7,[]],[le1,[]],[le2,[]]|TrL])).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(init(l1,Init)).

%%......................

:- fsa_regex_compile(file(bad),BAD),assertz(bad(BAD)).

:- fsa_regex_compile(file(good_result),GOOD),assertz(good(GOOD)).

%%%%%%%%%%%%%%%%%%%%%%%%





%% :- good(Good),init(Init),assertz(pred_lang([[Init,[0]],[Good,[0]]])).

%% :- good(Good),Good=fa(_,_,_,GoodFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Good,GoodFL]])).





%% :- bad(Bad),init(Init),assertz(pred_lang([[Init,[0]],[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,BadFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Bad,BadFL]])).





%% :- init(Init),assertz(pred_lang([[Init,[0]]])).





%% :- good(Good),assertz(pred_lang([[Good,[0]]])).

%% :- good(Good),Good=fa(_,_,_,GoodFL,_,_),assertz(pred_lang([[Good,GoodFL]])).





%% :- bad(Bad),assertz(pred_lang([[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,FL,_,_),assertz(pred_lang([[Bad,FL]])).





%%......................
%%%%%%%%%%%%%%%
%%%%%%%%%

%%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%%......................





%%......................

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(2).
max_for_appl_tuples(10).

%%......................

%% Abstraction based on forward collapsing:





%%......................
%%......................

%% Checking whether the abstraction is an overapproximation of the concrete state set.

%% abstract(State,AbsState) :-
%% abstr_fw(State,AbsState),
%% fsa_regex_compile(intersect(complement(fa(AbsState)),fa(State)),X),
%% ( (empty(X), !,
%% write('Abstraction is an overapproximation!'),nl);
%% (write('Abstraction is NOT an overapproximation!'),nl,
%% fsa_write_file(xxxState,State),
%% fsa_write_file(xxxAbsState,AbsState),
%% trace) ).

%% Testing emptiness of intersectins of abstractions and predicates:
%% Requires new_pred to be initially asserted (see below) and its use enabled in the sources.

%% abstract(State,AbsState) :- abstr_fw_debug(State,AbsState).

%%......................

%% Abstraction based on forward/backward collapsing:

%% abstract(State,AbsState) :- abstr_fwbw(State,AbsState).

%% Abstraction based on forward/backward collapsing - testing disjunctness of abstractions and bad sets

%% abstract(State,AbsState) :- abstr_fwbw_debug(State,AbsState).

%%......................

%% Abstraction based on backward collapsing:





%% Checking whether the abstraction is an overapproximation of the concrete state set.

%% abstract(State,AbsState) :-
%% abstr_bw(State,AbsState),
%% fsa_regex_compile(intersect(complement(fa(AbsState)),fa(State)),X),
%% ( (empty(X), !,
%% write('Abstraction is an overapproximation!'),nl);
%% (write('Abstraction is NOT an overapproximation!'),nl,
%% fsa_write_file(xxxState,State),
%% fsa_write_file(xxxAbsState,AbsState),
%% trace) ).

%% Abstraction based on forward/backward collapsing - testing disjunctness of abstractions and bad sets

%% abstract(State,AbsState) :- abstr_bw_debug(State,AbsState).

%%......................

%% Abstraction based on fw/bw languages with a bounded length of words/sets of traces of a bounded length:





%%......................
%%......................





%%......................





%%......................
%%......................





%%......................





%%......................

:- sort([!,#,l,x,y,z,'_f','_t',n,e,s,su,u,/,'|',l1,l2,l3,l4,l5,l6,l7,le1,le2,m_f,m_t],S),assertz(sigma(S)).
%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic with_self_loop_acclr/0, new_pred/1.

%% with_self_loop_acclr.

%% Just for debugging the collapsing method...

new_pred([]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%


:- assertz(unwound_times(2)).

abstract(State,AbsState) :- accel_unwound_self_loops(/,State,AbsState).

%%% A version for debugging:

%% abstract(State,AbsState) :-
%% accel_unwound_self_loops(/,State,AbsState),
%% fsa_write_file(xxx,State),
%% fsa_write_file(yyy,AbsState),
%% true.



%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%







%%%%%%%%%%%%%%%%%%%%%%%%
