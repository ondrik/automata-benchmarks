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
  fsa_regex_compile(intersect(fa(Abs),fa(Pred)),1),
  ( (empty(1), !,
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
  fsa_regex_compile(intersect(fa(Abs),fa(Pred)),1),
  ( (empty(1), !,
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
  fsa_regex_compile(intersect(fa(Pred),fa(AbsState)),1),
  ( (empty(1), !, do_check_coll(PredL,State,AbsState));
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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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
  fsa_regex_compile(minimize(fa(Abstr1)),Abstr).

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
  fsa_regex_compile(minimize(fa(FA1)),FA2).

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

add_new_pred(CritConcState,NewPred) :-
  run(N),
  NewPred=fa(_,Size,_,_,_,_),
  write('In run '),write(N),write(', adding a new pred. aut. with '),write(Size),write(' states.'),nl,
  name(N,NL),atom_chars(PN,NL),atom_concat('npr',PN,PP),fsa_write_file(PP,NewPred),
  %%% !!!!! BEWARE OF USING THE RIGHT VERSION OF prepare_summary_bad !!!!!
  %%% Use summ_bad:-pred_lang if both of the following hold:
  %%% (1) New predicates are being united with the old ones.
  %%% (2) Only the set of bad states is used as the initial predicate.
  %%% !!!!! BEWARE OF SWITCHING THIS CORRECTLY !!!!!

  retract(pred_lang(PredLangL)), %% Must be switched off for abstr_fl, but on otherwise...

  %-------------

  assertz(pred_lang([NewPred|PredLangL])), %% Choose this or vvv
    augment_summary_bad(NewPred), %% May be switched off.

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
  %% retract(fin_lang_up_to(OldLim)), %% !!! Switch off retract(pred_lang(PredLangL)) %% Choose this or ^^^
  %% %% NewLim is OldLim+Size, %% Choose this or vvv
  %% %% NewLim is OldLim+(Size//2), %% Choose this or ^^^
  %% write('Increasing the limit on the lenghts of the words considered: '),write(OldLim),write(' -> '),write(NewLim),nl,
  %% assertz(fin_lang_up_to(NewLim)),
  %% augment_summary_bad(NewPred), %% May be switched off.
  %-------------
  %% retract(new_pred(NewPredLangL)),assertz(new_pred([NewPred|NewPredLangL])),
  %% add_self_loops(NewPred,NewPredSelfLoops), %% Adding self-loops everywhere
  %% retract(pred_lang(PredLangL)),assertz(pred_lang([NewPredSelfLoops|PredLangL])),
  %% ( (with_self_loop_acclr, !,try_self_loop_acclr(N,[NewPred|NewPredLangL])); true ),
  !.

%-------------------------------------------------------------------------------

do_tr_reduce(trans(X,in(S1),Y),trans(X,in(S2),Y)) :- list_to_ord_set(S1,S2), !.
do_tr_reduce(trans(X,not_in(S1),Y),trans(X,not_in(S2),Y)) :- list_to_ord_set(S1,S2), !.
do_tr_reduce(X,X).

tr_reduce(fa(PrMod,NQ,IniL,FinL,TrL,[]),RedAtm) :-
  maplist(do_tr_reduce,TrL,NewTrL),
  fsa_regex_compile(minimize(fa(fa(PrMod,NQ,IniL,FinL,NewTrL,[]))),RedAtm).

%...............................................................................


%%% To be used with trans. rel. embedding stuttering (suitable for safety):

do_one_run(N,AbsReach,Result,Value) :-
  tr(TR),
  NN is N+1,
  write('Computing post...'),nl,
  AbsReach=fa(_,NS1,_,_,T1,_),length(T1,NT1),
  write('Abstract source automaton: '),write(NS1),write(','),write(NT1),nl,



  fsa_to_fst(AbsReach,AbsReachTr),
  fsa_regex_compile(compose(fa(AbsReachTr),fa(TR)),NewReachTr),
  fst_to_minfsa(NewReachTr,NewReach),

  NewReach=fa(_,NS2,_,_,T2,_),length(T2,NT2),
  write('Concrete target automaton: '),write(NS2),write(','),write(NT2),nl,
  sum_bad(Bad),
  write('Intersecting with bad states...'),nl,
  fsa_regex_compile(intersect(fa(NewReach),fa(Bad)),ImmReachBad),
  %% fsa_write_file('error_reached',ImmReachBad),
  ( (empty(ImmReachBad), !,do_one_run_ph2(NN,AbsReach,NewReach,Result,Value));
    (Result=bad,Value=[ImmReachBad,ImmReachBad,NewReach],write('XXXXXXXXX'),nl) ).

%%% To be used with trans. rel. NOT embedding stuttering (suitable for liveness):

%% do_one_run(N,AbsReach,Result,Value) :-
%% tr(TR),
%% NN is N+1,
%% write('Computing post...'),nl,
%% AbsReach=fa(_,NS1,_,_,T1,_),length(T1,NT1),
%% write('Abstract source automaton: '),write(NS1),write(','),write(NT1),nl,
%% fsa_regex_compile(minimize(range(compose(fa(AbsReach),fa(TR)))),OneStepReach),
%% OneStepReach=fa(_,NS2,_,_,T2,_),length(T2,NT2),
%% write('Concrete target automaton: '),write(NS2),write(','),write(NT2),nl,
%% sum_bad(Bad),
%% write('Intersecting with bad states...'),nl,
%% fsa_regex_compile(intersect(fa(OneStepReach),fa(Bad)),ImmReachBad),
%% %% fsa_write_file('error_reached',ImmReachBad),
%% ( (empty(ImmReachBad),
%% !,
%% fsa_regex_compile(union(fa(AbsReach),fa(OneStepReach)),NewReach),
%% do_one_run_ph2(NN,AbsReach,NewReach,Result,Value));
%% (Result=bad,Value=[ImmReachBad,ImmReachBad,OneStepReach],write('XXXXXXXXX'),nl) ).

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
  sum_bad(Bad),
  write('Intersecting the abstraction with bad states...'),nl,
  fsa_regex_compile(intersect(fa(NewAbsReach),fa(Bad)),AbsImmBad),
  ( (empty(AbsImmBad), !,
     ( (AbsReach=NewAbsReach, !,Result=good,Value=NewAbsReach); %% Returning the fixpoint
       (write('>>> '),write(N),write(' >>>'),nl,
        %% name(N,NL),atom_chars(PN,NL),atom_concat('step',PN,PP),fsa_write_file(PP,NewAbsReach),
        do_one_run_ph3(N,NewReach,NewAbsReach,Result,Value)) ));
    (write('YYYYYYYYY'),nl,
     do_one_run_ph4(N,NewReach,NewAbsReach,AbsImmBad,NewReach,Result,Value)) ).

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



     fsa_to_fst(NextReachBad1,NextReachBad1Tr),
     fsa_regex_compile(compose(fa(NextReachBad1Tr),fa(ITR)),SrcBadTr),
     fst_to_minfsa(SrcBadTr,SrcBad),

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

%%% Choose one of the implementations (running always from the very beginning or computing incrementally):

%% do_one_run_ph4(N,NewReach,NewAbsReach,NewPred,NextReach,Result,Value) :- Result=new,Value=[NextReach,NewPred], !.

do_one_run_ph4(N,NewReach,NewAbsReach,NewPred,NextReach,Result,Value) :-
  add_new_pred(NextReach,NewPred),
  retract(run(Run)),
  NewRun is Run+1,
  assertz(run(NewRun)),
  write('+++++ Run '),write(NewRun),write(':'),nl,
  write('>>> '),write(N),write(' >>>'),nl,
  do_one_run_ph3(N,NewReach,NewAbsReach,Result,Value),
  !.

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



     fsa_to_fst(NextReachBad1,NextReachBad1Tr),
     fsa_regex_compile(compose(fa(NextReachBad1Tr),fa(ITR)),SrcBadTr),
     fst_to_minfsa(SrcBadTr,SrcBad),

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
    (Value=[CritConcState,NewPred],add_new_pred(CritConcState,NewPred),do_asmc) ),
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



  write('Dealing with R* (and not R*(I)) having been done Good is not currently implemented...'),nl,
  write('There could (?) be troubles with the complement to Bad, better do it manually...'),nl,
  halt,

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

  retract(init(Init)),
    fst_to_minfsa(Init,InitFsa),
    assertz(init(InitFsa)),
  retract(bad(Bad)),
    fst_to_minfsa(Bad,BadFsa),
    assertz(bad(BadFsa)),

  retract(pred_lang(PL)),
    maplist(fst_to_minfsa,PL,PFsaL),
    assertz(pred_lang(PFsaL)),


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

  retract(init(Init)),
    fst_to_minfsa(Init,InitFsa),
    assertz(init(InitFsa)),
  retract(bad(Bad)),
    fst_to_minfsa(Bad,BadFsa),
    assertz(bad(BadFsa)),

  retract(pred_lang(PL)),
    maplist(fst_to_minfsa,PL,PFsaL),
    assertz(pred_lang(PFsaL)),


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

%% fsa_regex_compile(minimize(fa(fa(X,S,I,F,T2,Y))),A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% That is all falks...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constract a list [0,1,...,N] from N

finallist(N,Final) :- !,
  finallist(N,[],Final).

finallist(0,L,[0|L]) :- !.
finallist(N,L,L1) :- N1 is N-1,finallist(N1,[N|L],L1).

% construct transitions trans(0,not_in...,1), trans(1,not_in,2), etc. up to N

translist(N,L) :-
  translist(N,[],L).

translist(0,L,L) :- !.
translist(N,L,L1) :- !, N1 is N-1,translist(N1,[trans(N1,not_in([]),N)|L],L1).

% produces automaton which accepts all strings up to size N

getautomatauptoN(N,fa(r(fsa_preds),N1,[0],Final,Trans,[])) :-
  N1 is N+1,
  finallist(N,Final),
  translist(N,Trans).

getautomatauptoN2(N,fa(r(fsa_preds),N1,[0],[N],Trans,[])) :-
  N1 is N+1,
  translist(N,Trans).

% generates all languages of each state of an automaton
% in the form [automaton,number of state]
% automaton must be deterministic

generate_subautomata(N,fa(P,N1,Init,Final,Trans,Jumps),L) :-
  getautomatauptoN(N,AN),
  generate_subautomata(AN,0,N1,fa(P,N1,Init,Final,Trans,Jumps),[],L).

generate_subautomata(AN,N,N,A,L,L) :- !.

generate_subautomata(AN,N1,N,fa(P,N,Init,Final,Trans,Jumps),L,L1) :- !,
     N2 is N1 + 1,
     fsa_regex_compile(mb(intersect(fa(AN),fa(fa(P,N,[N1],Final,Trans,Jumps)))),B),
     generate_subautomata(AN,N2,N,fa(P,N,Init,Final,Trans,Jumps),
                         [[B,N1]|L],L1).

% Version with traces (all states are final)

generate_subautomata2(N,fa(P,N1,Init,Final,Trans,Jumps),L) :-
  getlistupto(N1,LFinal),getautomatauptoN(N,AN),
  generate_subautomata2(LFinal,AN,0,N1,fa(P,N1,Init,Final,Trans,Jumps),[],L).

generate_subautomata2(_,AN,N,N,A,L,L) :- !.

generate_subautomata2(LFinal,AN,N1,N,fa(P,N,Init,Final,Trans,Jumps),L,L1) :- !,
     N2 is N1 + 1,
     fsa_regex_compile(mb(intersect(fa(AN),fa(fa(P,N,[N1],LFinal,Trans,Jumps)))),B),
% could be changed to take predecessors instead of succesors
% fsa_regex_compile(mb(intersect(reverse(fa(AN)),fa(fa(P,N,[N1],LFinal,Trans,Jumps)))),B2),
     generate_subautomata2(LFinal,AN,N2,N,fa(P,N,Init,Final,Trans,Jumps),
                         [[B,N1]|L],L1).

get_one([X,N],[[X,N1]|L],Rest,[N1|Res]) :- !,
   get_one([X,N],L,Rest,Res).

get_one([_,N],Rest,Rest,[N]).

get_part(X,Y) :- get_part(X,[],Y).

get_part([],Part,Part) :- !.

get_part([X|L],Part1,Part) :-
   get_one(X,L,L1,P),
   get_part(L1,[P|Part1],Part).

get_partition(L,Part) :-
   sort(L,L1),
   get_part(L1,Part).

getInit([X|L],[X]) :- member(0,X), !.
getInit([_|L],Part) :- getInit(L,Part).

getstate(M,[X|L],X) :- member(M,X), !.
getstate(M,[_|L],Part) :- getstate(M,L,Part).

addtolist(X,[],[X]) :- !.
addtolist(X,[X|L],[X|L]) :- !.
addtolist(X,[Y|L],[Y|L1]) :- addtolist(X,L,L1).

getFinal(Part,Final,Final1) :- getFinal(Part,Final,[],Final1).
getFinal(_,[],L,L) :- !.
getFinal(Part,[X|Final],L,L1) :-
   getstate(X,Part,P),
   addtolist(P,L,L2),
   getFinal(Part,Final,L2,L1).

getTrans(_,[],[]) :- !.
getTrans(Part,[trans(Q,E,Q1)|L],[trans(Q2,E,Q3)|L1]) :-
   getstate(Q,Part,Q2),
   getstate(Q1,Part,Q3),
   getTrans(Part,L,L1).

collapse_partition(fa(P,N,Init,Final,Trans,Jumps),Part,
                        Init1,Final1,Trans1) :-
   getInit(Part,Init1),
   getFinal(Part,Final,Final1),
   getTrans(Part,Trans,Trans1).

% collapse(_,A,A1) :- sminimize_hopcroft(A,A2),
% fsa_regex_compile(minimize(fa(A2)),A1).

collapse(N,A,A1) :- flag(trace),!,collapse2(N,A,A1).

collapse(N,A,A1) :- collapse1(N,A,A1).

collapse1(N,A,A1) :-
   generate_subautomata(N,A,L),
   get_partition(L,Part),
   collapse_partition(A,Part,Init,Final,Trans),
   fsa_construct_rename_states(r(fsa_preds),Init,Final,Trans,[],A2),
   fsa_regex_compile(mb(fa(A2)),A1).

collapse2(N,A,A1) :-
   generate_subautomata2(N,A,L),
   get_partition(L,Part),
   collapse_partition(A,Part,Init,Final,Trans),
   fsa_construct_rename_states(r(fsa_preds),Init,Final,Trans,[],A2),
   fsa_regex_compile(mb(fa(A2)),A1).

getlistupto(0,[0]) :- !.
getlistupto(N1,[N1|L1]) :- N2 is N1 - 1,getlistupto(N2,L1).

getfixpoint(Init,Transducer,Res) :-
   onestep(Init,Transducer,S1),
   getfixpoint(Init,S1,Transducer,Res).

getfixpoint(S1,S1,Transducer,S1) :- !.

getfixpoint(_,S2,Transducer,Res) :-
   onestep(S2,Transducer,S3),
   getfixpoint(S2,S3,Transducer,Res).

onestep(Init,Transducer,Res) :-
% fsa_regex_compile(mb(range(compose_list([fa(Init),fa(Transducer),fa(Transducer),fa(Transducer)]))),Res).
 fsa_regex_compile(mb(range(compose(fa(Init),fa(Transducer)))),Res).

getfixpointfile(Init,Trans,Res) :-
   fsa_regex_compile(file(Init),I), fsa_regex_compile(file(Trans),T),
   getfixpoint(I,T,R),
   fsa_write_file(Res,R).

getcollapsefile(N,File,Res) :-
fsa_regex_compile(file(File),A),collapse(N,A,A1),fsa_write_file(Res,A1).

forwardcomp(N,Init,Transducer,Res) :-
   onestepforward(N,Init,Transducer,S1),
   forwardcomp(N,Init,S1,Transducer,Res,0).

forwardcomp(_,S1,S1,Transducer,S1,_) :- !.

forwardcomp(N,_,S2,Transducer,Res,N1) :-
   write('Step: '),write(N1),nl,
   onestepforward(N,S2,Transducer,S3),
   N2 is N1 + 1,
   forwardcomp(N,S2,S3,Transducer,Res,N2).

onestepforward(N,Init,Transducer,Res) :-
% fsa_regex_compile(mb(range(compose_list([fa(Init),fa(Transducer),fa(Transducer),fa(Transducer)]))),Res1),
 fsa_regex_compile(mb(range(compose(fa(Init),fa(Transducer)))),Res1),
   collapse(N,Res1,Res).

forwardcompfile(N,Init,Trans,Res) :-
   statistics(cputime,[T1,_]),
   fsa_regex_compile(file(Init),I), fsa_regex_compile(file(Trans),T),
   forwardcomp(N,I,T,R),
   fsa_write_file(Res,R),
   statistics(cputime,[T2,_]),
   Time is (T2-T1)/1000,
   write('Cputime used: '),write(Time),write(' sec.'),nl.

%%%%%%%%%% Different way to calculate partition, not used not finished

generatepart(N,N,Final,Part1,Part1) :- !.
generatepart(N1,N,Final,Part1,Part2) :-
   N2 is N1 + 1,
    ( (member(N1,Final), ! , generatepart(N2,N,Final,Part1,Part2)) ;
       generatepart(N2,N,Final,[N1|Part1],Part2)).

calculate_partition(0,fa(_,N,Init,Final,Trans,_),_,Final,Part1) :-
   generatepart(0,N,Final,[],Part1).

%calculate_partition(N,A,Part,Part1) :-

% Method of calculating first config up to size N, then collapsing
% for length preserving transducers

verify1t(N,Init,Trans,Prop) :-
     assertz(flag(trace)),
     verify1(N,Init,Trans,Prop),
     retractall(flag(trace)).

verify1(N,Init,Trans,Prop) :-
    statistics(cputime,[T1,_]),
    fsa_regex_compile(file(Init),I),
    fsa_regex_compile(file(Trans),T),
    fsa_regex_compile(complement(file(Prop)),Bad),
    verify11(N,I,T,Bad),
    statistics(cputime,[T2,_]),
   Time is (T2-T1)/1000,
   write('Cputime used: '),write(Time),write(' sec.'),nl.

verify11(N,I,T,Bad) :-
    write('Trying config up to '), write(N), write(' '),
    getautomatauptoN(N,A),
    fsa_regex_compile(intersect(fa(I),fa(A)),A1),
    getfixpoint(A1,T,Res),
    collapsing(0,N,Res,T,Bad,I).

verify11(N,Init,Trans,Bad) :-
    N1 is N + 1,
    verify11(N1,Init,Trans,Bad).

collapsing(N,N1,Res,T,Bad,I) :-
    write('with '), write(N), write('-equivalence '),nl,
    N1 > N,
    collapse(N,Res,Res1),
    checkinclusion(I,Res1),
    write(inclusion),
% fsa_regex_compile(union(fa(Res1),fa(I)),Res2),
    checkfixpoint(Res1,T),
    write(fixpoint),
    checkproperty(Res1,Bad),
    fsa_write_file(resultat,Res1).

collapsing(N,N1,Res,T,Bad,I) :-
   N2 is N + 1, N1 > N2, !,
   collapsing(N2,N1,Res,T,Bad,I).

checkinclusion(I,Res) :-
    fsa_regex_compile(intersect(fa(I),complement(fa(Res))),A),
    is_empty(A).

checkequivalent(A,B) :-
    fsa_regex_compile(union(intersect(fa(A),complement(fa(B))),
                        intersect(complement(fa(A)),fa(B))),C),
    is_empty(C).

checkfixpoint(A,T) :-
   fsa_regex_compile(mb(range(compose(fa(A),fa(T)))),Res1),
   checkequivalent(Res1,A).

checkproperty(A,Bad) :-
   fsa_regex_compile(intersect(fa(A),fa(Bad)),C),
   is_empty(C), !, write('Property verified'),nl.

is_empty(fa(_,1,[0],[],_,_)).

verify2t(N,Init,Trans,Prop) :-
     assertz(flag(trace)),
     verify2(N,Init,Trans,Prop),
     retractall(flag(trace)).

verify2(N,Init,Trans,Prop) :-
    statistics(cputime,[T1,_]),
    fsa_regex_compile(file(Init),I),
    fsa_regex_compile(file(Trans),T),
    fsa_regex_compile(complement(file(Prop)),Bad),
    verify21(N,I,T,Bad),
    statistics(cputime,[T2,_]),
   Time is (T2-T1)/1000,
   write('Cputime used: '),write(Time),write(' sec.'),nl.

verify21(N,I,T,Bad) :-
    write('Trying config up to '), write(N), nl,
    getautomatauptoN(N,A),
    getfixpointrestricted(A,I,T,Res),
    collapsing(0,N,Res,T,Bad,I).

verify21(N,Init,Trans,Bad) :-
    N1 is N + 1,
    verify21(N1,Init,Trans,Bad).

getfixpointrestricted(A,Init,Transducer,Res) :-
   onesteprestricted(A,Init,Transducer,S1),
   getfixpointrestricted(A,Init,S1,Transducer,Res).

getfixpointrestricted(_,S1,S1,Transducer,S1) :- !.

getfixpointrestricted(A,_,S2,Transducer,Res) :-
   onesteprestricted(A,S2,Transducer,S3),
   getfixpointrestricted(A,S2,S3,Transducer,Res).

onesteprestricted(A,Init,Transducer,Res) :-
   fsa_regex_compile(mb(intersect(range(compose(fa(Init),fa(Transducer))),fa(A))),Res).

verify3(N,Init,Trans,Prop) :-
    statistics(cputime,[T1,_]),
    fsa_regex_compile(file(Init),I),
    fsa_regex_compile(file(Trans),T),
    fsa_regex_compile(complement(file(Prop)),Bad),
    verify31(N,I,T,Bad),
    statistics(cputime,[T2,_]),
   Time is (T2-T1)/1000,
   write('Cputime used: '),write(Time),write(' sec.'),nl.

verify31(N,I,T,Bad) :-
    write('Trying '), write(N), write('-equivalence'), nl,
    forwardcomp(N,I,T,Res),
    checkproperty(Res,Bad), fsa_write_file(resultat,Res),!.

verify31(N,Init,Trans,Bad) :-
    N1 is N + 1,
    verify31(N1,Init,Trans,Bad).

verify3t(N,Init,Trans,Prop) :-
     assertz(flag(trace)),
     verify3(N,Init,Trans,Prop),
     retractall(flag(trace)).

%list_profile :-
% % get number of calls for each profiled procedure
% setof(D-[M:P|D1],(current_module(M),profile_data(M:P,calls,D),profile_data(M:P,retries,D1)),LP),
% % output so that the most often called
% % predicates will come last:
% write_profile_data(LP).
%
% list_profile(Module) :-
% % get number of calls for each profiled procedure
% setof(D-[Module:P|D1],(profile_data(Module:P,calls,D),profile_data(Module:P,retries,D1)),LP),
% % output so that the most often called
% % predicates will come last:
% write_profile_data(LP).
%
% write_profile_data([]).
% write_profile_data([D-[M:P|R]|SLP]) :-
% % swap the two calls if you want the most often
% % called predicates first.
% format('~a:~w: ~32+~t~d~12+~t~d~12+~n', [M,P,D,R]),
% write_profile_data(SLP).
%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the abstr_collprice of a slower run...

%% :- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).


:- fsa_global_set(pred_module,fsa_frozen).


%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init_rev),Init),assertz(init(Init)).

%%......................

:- fsa_regex_compile(file(trs),TR),assertz(itr(TR)).

%%......................

:- fsa_regex_compile(file(not_rev_list_rev),BAD),assertz(bad(BAD)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),init(Init),assertz(pred_lang([Init,Good])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),init(Init),assertz(pred_lang([[Init,[0]],[Good,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),
%% Good=fa(_,_,_,GoodFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Good,GoodFL]])).

%% :- bad(Bad),init(Init),assertz(pred_lang([Init,Bad])).

%% :- bad(Bad),init(Init),assertz(pred_lang([[Init,[0]],[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,BadFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Bad,BadFL]])).





%% :- init(Init),assertz(pred_lang([[Init,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([Good])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([[Good,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),Good=fa(_,_,_,GoodFL,_,_),assertz(pred_lang([[Good,GoodFL]])).


:- bad(Bad),assertz(pred_lang([Bad])).


%% :- bad(Bad),assertz(pred_lang([[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,FL,_,_),assertz(pred_lang([[Bad,FL]])).

%%......................



:- retract(pred_lang(PL)),fsa_regex_compile(file(pr1_rev),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr2_rev),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr3_rev),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr4_rev),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr5_rev),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr6_rev),P),assertz(pred_lang([P|PL])).



%%%%%%%%%

%%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(2).
max_for_appl_tuples(10).

%%......................

%% Abstraction based on forward collapsing:


abstract(State,AbsState) :- abstr_fw(State,AbsState).


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
:- sort([<,>,e,i,il,ilx,ilxy,ily,ix,ixy,iy,o,ol,olx,olxy,oly,ox,oxy,oy,q0,q1,q2,q3,q4,q5,q6,q7,'|'],S),assertz(sigma(S)).
%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic with_self_loop_acclr/0, new_pred/1.

%% with_self_loop_acclr.

%% Just for debugging the collapsing method...

new_pred([]).

%%%%%%%%%%%%%%%%%%%%%%%%
