%%%%%%%%%%%%%%%%%%%%%%%%

%% :- consult('/usr/local/src/FSA/src-compiled-swi/fsa_library').

%% :- consult('/usr/local/src/FSA/src-compiled-yap/fsa_library').
:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the price of a slower run...

:- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(init(l1,Init)).

%%%%%%%%%%%%%%%%%%%%%%%%

:- assertz(tr([])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line01_to_line01_02),L1L1_2),
   fsa_regex_compile(file(line01_to_line05),L1L5),
   assertz(tr([[l1,[[l2,L1L1_2],[l5,L1L5]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line02_to_line02_03),L2L2_3),
   assertz(tr([[l2,[[l3,L2L2_3]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line03_to_line03_04),L3L3_4),
   assertz(tr([[l3,[[l4,L3L3_4]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line04_to_line04_05),L4L4_5),
   assertz(tr([[l4,[[l5,L4L4_5]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line05_to_line05_06),L5L5_6),
   assertz(tr([[l5,[[l6,L5L5_6]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line06_to_line06_07),L6L6_7),
   assertz(tr([[l6,[[l7,L6L6_7]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line07_to_line07_19),L7L7_19),
   fsa_regex_compile(file(line07_to_line08),L7L8),
   assertz(tr([[l7,[[l19,L7L7_19],[l8,L7L8]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line08_to_line08_09),L8L8_9),
   fsa_regex_compile(file(line08_to_line11),L8L11),
   assertz(tr([[l8,[[l9,L8L8_9],[l11,L8L11]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line09_to_line09_10),L9L9_10),
   assertz(tr([[l9,[[l10,L9L9_10]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line10_to_line10_13),L10L10_13),
   assertz(tr([[l10,[[l13,L10L10_13]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line11_to_line11),L11L11),
   fsa_regex_compile(file(line11_to_line12),L11L12),
   assertz(tr([[l11,[[l11,L11L11],[l12,L11L12]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line12_to_line12_13),L12L12_13),
   assertz(tr([[l12,[[l13,L12L12_13]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line13_to_line13_14),L13L13_14),
   assertz(tr([[l13,[[l14,L13L13_14]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line14_to_line14_15),L14L14_15),
   assertz(tr([[l14,[[l15,L14L14_15]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line15_to_line15_07),L15L15_7),
   fsa_regex_compile(file(line15_to_line16),L15L16),
   assertz(tr([[l15,[[l7,L15L15_7],[l16,L15L16]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line16_to_line16_17),L16L16_17),
   assertz(tr([[l16,[[l17,L16L16_17]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line17_to_line17_18),L17L17_18),
   assertz(tr([[l17,[[l18,L17L17_18]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line18_to_line18_07),L18L18_7),
   assertz(tr([[l18,[[l7,L18L18_7]]]|TrL])).

:- retract(tr(TrL)),
   assertz(tr([[l19,[]]|TrL])).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(bad),BAD),assertz(bad(BAD)).

%%......................

:- fsa_regex_compile(file(good_result),GOOD),assertz(good(GOOD)).

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef PrInitGood
:- good(Good),init(_,Init),assertz(pred_lang([Init,Good])).
#endif

%% :- good(Good),init(_,Init),assertz(pred_lang([[Init,[0]],[Good,[0]]])).

%% :- good(Good),Good=fa(_,_,_,GoodFL,_,_),init(_,Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Good,GoodFL]])).

#ifdef PrInitBad
:- bad(Bad),init(_,Init),assertz(pred_lang([Init,Bad])).
#endif

%% :- bad(Bad),init(_,Init),assertz(pred_lang([[Init,[0]],[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,BadFL,_,_),init(_,Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Bad,BadFL]])).

#ifdef PrInit
:- init(_,Init),assertz(pred_lang([Init])).
#endif

%% :- init(_,Init),assertz(pred_lang([[Init,[0]]])).

#ifdef PrGood
:- good(Good),assertz(pred_lang([Good])).
#endif

%% :- good(Good),assertz(pred_lang([[Good,[0]]])).

%% :- good(Good),Good=fa(_,_,_,GoodFL,_,_),assertz(pred_lang([[Good,GoodFL]])).
   
#ifdef PrBad
:- bad(Bad),assertz(pred_lang([Bad])).
#endif

%% :- bad(Bad),assertz(pred_lang([[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,FL,_,_),assertz(pred_lang([[Bad,FL]])).

#ifdef PrInitGoodBad
:- init(_,Init),good(Good),bad(Bad),assertz(pred_lang([Init,Good,Bad])).
#endif

%%......................

#ifdef PrGuards


#endif

%%%%%%%%%%%%%%%

#ifdef PrActions


#endif

%%%%%%%%%%%%%%%

#ifdef PrSpecial

%% :- fsa_regex_atom_compile('{}',Empty), assertz(pred_lang([Empty])).

:- ((retract(pred_lang(PL)),!);PL=[]),fsa_regex_compile(file(pr_slash),P),assertz(pred_lang([P|PL])).

%% :- ((retract(pred_lang(PL)),!);PL=[]),fsa_regex_compile(file('bad-pure_mem_cons'),P),assertz(pred_lang([P|PL])).

#endif

%%%%%%%%%

%%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%%......................

#ifdef NoAbs
abstract(State,State).
#endif

%%......................

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(2).
max_for_appl_tuples(10).

%%......................

%% Abstraction based on forward collapsing:

#ifdef FwColl
abstract(State,AbsState) :- abstr_fw(State,AbsState).
#endif

%%......................

#ifdef FwCollInv

:- fsa_regex_compile(file(inv),Inv),assertz(invariant(Inv)).

abstract(State,AbsStateInv) :- 
  abstr_fw(State,AbsState),
  invariant(Inv),
  fsa_regex_compile(intersect(fa(AbsState),fa(Inv)),AbsStateInv).

#endif

%%......................

%% Checking whether the abstraction is an overapproximation of the concrete state set.

%% abstract(State,AbsState) :-
%%   abstr_fw(State,AbsState),
%%   fsa_regex_compile(intersect(complement(fa(AbsState)),fa(State)),X),
%%   ( (empty(X), !,
%% 	write('Abstraction is an overapproximation!'),nl);
%%     (write('Abstraction is NOT an overapproximation!'),nl,
%% 	fsa_write_file(xxxState,State),
%% 	fsa_write_file(xxxAbsState,AbsState),
%% 	trace) ).

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

#ifdef BwColl
abstract(State,AbsState) :- abstr_bw(State,AbsState).
#endif

%% Checking whether the abstraction is an overapproximation of the concrete state set.

%% abstract(State,AbsState) :-
%%   abstr_bw(State,AbsState),
%%   fsa_regex_compile(intersect(complement(fa(AbsState)),fa(State)),X),
%%   ( (empty(X), !,
%% 	write('Abstraction is an overapproximation!'),nl);
%%     (write('Abstraction is NOT an overapproximation!'),nl,
%% 	fsa_write_file(xxxState,State),
%% 	fsa_write_file(xxxAbsState,AbsState),
%% 	trace) ).

%% Abstraction based on forward/backward collapsing - testing disjunctness of abstractions and bad sets

%% abstract(State,AbsState) :- abstr_bw_debug(State,AbsState).

%%......................

%% Abstraction based on fw/bw languages with a bounded length of words/sets of traces of a bounded length:

#ifdef FlColl
abstract(State,AbsState) :- abstr_fl(State,AbsState).  %% Choose this or vvv
#endif

%%......................

#ifdef FlCollInv

:- fsa_regex_compile(file(inv),Inv),assertz(invariant(Inv)).

abstract(State,AbsStateInv) :- 
  abstr_fl(State,AbsState),
  invariant(Inv),
  fsa_regex_compile(intersect(fa(AbsState),fa(Inv)),AbsStateInv).

#endif

%%......................

#ifdef FtColl
abstract(State,AbsState) :- abstr_ft(State,AbsState).  %% Choose this or ^^^
#endif

%%......................

#ifdef FlRefByFtColl
abstract(State,AbsState) :- abstr_fl_refby_ft(State,AbsState).  %% Choose this or ^^^
#endif

%%......................

#ifdef FlRefByFtCollInv

:- fsa_regex_compile(file(inv),Inv),assertz(invariant(Inv)).

abstract(State,AbsStateInv) :- 
  abstr_fl_refby_ft(State,AbsState),
  invariant(Inv),
  fsa_regex_compile(intersect(fa(AbsState),fa(Inv)),AbsStateInv).

#endif

%%......................

#ifdef FbColl
abstract(State,AbsState) :- abstr_fb(State,AbsState).	%% Choose this or vvv
#endif

%%......................

#ifdef FbtColl
abstract(State,AbsState) :- abstr_ftb(State,AbsState).  %% Choose this or ^^^
#endif

%%......................

:- sort([!,#,/,'|','_f','_t',n,s,su,e,u,
         l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,
         a,b,x,y,p,t,r],S),assertz(sigma(S)).

#ifdef IniBad
:- bad(fa(_,QN,_,_,_,_)),assertz(fin_lang_up_to(QN)).
#endif

#ifdef IniHalfBad
:- bad(fa(_,QN,_,_,_,_)),Lim is QN//2,assertz(fin_lang_up_to(Lim)).
#endif

#ifdef IniOne
:- assertz(fin_lang_up_to(1)).
#endif

#ifdef IniSp
:- assertz(fin_lang_up_to(IniSp)).
#endif

#ifdef IniInit
:- init(_,Init),Init=fa(_,QI,_,_,_,_),assertz(fin_lang_up_to(QI)).
#endif

#ifdef IniHalfInit
:- init(_,Init),Init=fa(_,QI,_,_,_,_),Lim is QI//2,assertz(fin_lang_up_to(Lim)).
#endif

%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic with_self_loop_acclr/0, new_pred/1.

%% with_self_loop_acclr.

%% Just for debugging the collapsing method...

new_pred([]).

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef AddIniBad
:- bad(fa(_,QN,_,_,_,_)),assertz(fin_lang_up_to(QN)).
#endif

#ifdef AddIniHalfBad
:- bad(fa(_,QN,_,_,_,_)),Lim is QN//2,assertz(fin_lang_up_to(Lim)).
#endif

#ifdef AddIniOne
:- assertz(fin_lang_up_to(1)).
#endif

#ifdef AddIniSp
:- assertz(fin_lang_up_to(IniSp)).
#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef AccelViaUnwSelfLoops
:- assertz(unwound_times(UnwTimes)).

abstract(State,AbsState) :- accel_unwound_self_loops(LoopLetter,State,AbsState).

%%% A version for debugging:

%% abstract(State,AbsState) :- 
%%   accel_unwound_self_loops(LoopLetter,State,AbsState),
%%   fsa_write_file(xxx,State),
%%   fsa_write_file(yyy,AbsState),
%%   true.

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef AccelViaUnwSelfLoopsInv
:- assertz(unwound_times(UnwTimes)).
:- fsa_regex_compile(file(inv),Inv),assertz(invariant(Inv)).

abstract(State,AbsStateInv) :- 
  accel_unwound_self_loops(LoopLetter,State,AbsState),
  invariant(Inv),
  fsa_regex_compile(intersect(fa(AbsState),fa(Inv)),AbsStateInv).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef InvAccelViaUnwSelfLoopsInv
:- assertz(unwound_times(UnwTimes)).
:- fsa_regex_compile(file(inv),Inv),assertz(invariant(Inv)).

abstract(State,AbsStateInv) :- 
  invariant(Inv),
  fsa_regex_compile(intersect(fa(State),fa(Inv)),StateInv),  
  accel_unwound_self_loops(LoopLetter,StateInv,AbsState),
  fsa_regex_compile(intersect(fa(AbsState),fa(Inv)),AbsStateInv).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef AccelViaUnwLoops
:- assertz(unwound_times(UnwTimes)).

abstract(State,AbsState) :- accel_unwound_loops(LoopL,State,AbsState).

%%% A version for debugging:

%% abstract(State,AbsState) :- 
%%   accel_unwound_loops(LoopL,State,AbsState),
%%   fsa_write_file(xxx,State),
%%   fsa_write_file(yyy,AbsState),
%%   true.

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef AccelViaUnwLoopsInv
:- assertz(unwound_times(UnwTimes)).
:- fsa_regex_compile(file(inv),Inv),assertz(invariant(Inv)).

abstract(State,AbsStateInv) :- 
  accel_unwound_loops(LoopL,State,AbsState),
  invariant(Inv),
  fsa_regex_compile(intersect(fa(AbsState),fa(Inv)),AbsStateInv).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef InvAccelViaUnwLoopsInv
:- assertz(unwound_times(UnwTimes)).
:- fsa_regex_compile(file(inv),Inv),assertz(invariant(Inv)).

abstract(State,AbsStateInv) :- 
  invariant(Inv),
  fsa_regex_compile(intersect(fa(State),fa(Inv)),StateInv),  
  accel_unwound_loops(LoopL,StateInv,AbsState),
  fsa_regex_compile(intersect(fa(AbsState),fa(Inv)),AbsStateInv).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef AccelViaSeveralUnwLoops
:- assertz(unwound_times(UnwTimes)).

abstract(State,AbsState) :- accel_several_unwound_loops(LoopsL,State,AbsState).

%%% A version for debugging:

%% abstract(State,AbsState) :- 
%%   accel_several_unwound_loops(LoopL,State,AbsState),
%%   fsa_write_file(xxx,State),
%%   fsa_write_file(yyy,AbsState),
%%   true.

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef AccelViaAllLoopsFrom
:- AccelViaAllLoopsFrom=[SepSym,DtSymL],
   assertz(unwound_times(UnwTimes)),
   assertz(all_pos_loops_up_to(AllLoopsIniLen)),
   all_pos_loops_gen_loop(AllLoopsIniLen,DtSymL,DtSymL,SepSym,[],LoopsL),
   assertz(all_pos_loops(LoopsL)).

abstract(State,AbsState) :- 
  all_pos_loops(LoopsL),
  accel_several_unwound_loops(LoopsL,State,AbsState).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

% Automatically detecting loops of length (max_collapsable_seq)//(unw_times).

#ifdef AutoDetLoopsFrom
:- AutoDetLoopsFrom=[StartUnwTimes,SepSym,DtSymL],
   assertz(aut_loop_det_from(SepSym,DtSymL)),
   assertz(unwound_times(StartUnwTimes)).

abstract(State,AbsState) :-
  aut_loop_det_from(SepSym,DtSymL),
  aut_loop_det(DtSymL,SepSym,State,AbsState).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef RefWrtRepSym

:- assertz(rep_sym(RefWrtRepSym)).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%
