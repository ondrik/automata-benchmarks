%%%%%%%%%%%%%%%%%%%%%%%%

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
  fsa_regex_compile(minimize(intersect(fa(A),complement(fa(B)))),E),
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




  A=B,

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



  ( (PredModule\=r(fsa_frozen), !, write('Predicate module fsa_frozen required!'),nl,halt); true ),

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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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



  ( (PredModule\=r(fsa_frozen), !, write('Predicate module fsa_frozen required!'),nl,halt); true ),

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



  ( (PredModule\=r(fsa_frozen), !, write('Predicate module fsa_frozen required!'),nl,halt); true ),

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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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



  ( (PredModule\=r(fsa_frozen), !, write('Predicate module fsa_frozen required!'),nl,halt); true ),

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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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
fl_reach_classes_t(Q1,X,Q2,_,ClL,ReachClL,NewReachClL) :-
  member([C,QL],ClL),member(Q2,QL),
  fl_add_reach_classes(Q1,[X],C,ReachClL,NewReachClL),
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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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
