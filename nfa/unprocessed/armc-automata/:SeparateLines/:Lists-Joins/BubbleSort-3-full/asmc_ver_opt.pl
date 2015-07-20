%%%%%%%%%%%%%%%%%%%%%%%%

%% :- consult('/usr/local/src/FSA/src-compiled-swi/fsa_library').

%% :- consult('/usr/local/src/FSA/src-compiled-yap/fsa_library').
:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the abstr_collprice of a slower run...

:- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(init(l1,Init)).

%%%%%%%%%%%%%%%%%%%%%%%%

:- assertz(tr([])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line01_to_line02),L1L2),
   assertz(tr([[l1,[[l2,L1L2]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line02_to_line03),L2L3),
   fsa_regex_compile(file(line02_to_line26),L2L26),
   assertz(tr([[l2,[[l3,L2L3],[l26,L2L26]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line03_to_line04),L3L4),
   assertz(tr([[l3,[[l4,L3L4]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line04_to_line05),L4L5),
   assertz(tr([[l4,[[l5,L4L5]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line05_to_line05_06),L5L5_6),
   assertz(tr([[l5,[[l6,L5L5_6]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line06_to_line06_07),L6L6_7),
   fsa_regex_compile(file(line06_to_line08),L6L8),
   assertz(tr([[l6,[[l7,L6L6_7],[l8,L6L8]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line07_to_line07_09),L7L7_9),
   assertz(tr([[l7,[[l9,L7L7_9]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line08_to_line08_09),L8L8_9),
   assertz(tr([[l8,[[l9,L8L8_9]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line09_to_line09_02),L9L9_2),
   fsa_regex_compile(file(line09_to_line10),L9L10),
   assertz(tr([[l9,[[l2,L9L9_2],[l10,L9L10]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line10_to_line11),L10L11),
   fsa_regex_compile(file(line10_to_line23),L10L23),
   assertz(tr([[l10,[[l11,L10L11],[l23,L10L23]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line11_to_line11_12),L11L11_12),
   assertz(tr([[l11,[[l12,L11L11_12]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line12_to_line13),L12L13),
   assertz(tr([[l12,[[l13,L12L13]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line13_to_line13),L13L13),
   fsa_regex_compile(file(line13_to_line14),L13L14),
   assertz(tr([[l13,[[l13,L13L13],[l14,L13L14]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line14_to_line14_15),L14L15),
   assertz(tr([[l14,[[l15,L14L15]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line15_to_line15),L15L15),
   fsa_regex_compile(file(line15_to_line16),L15L16),
   assertz(tr([[l15,[[l15,L15L15],[l16,L15L16]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line16_to_line16_17),L16L17),
   assertz(tr([[l16,[[l17,L16L17]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line17_to_line17_18),L17L17_18),
   fsa_regex_compile(file(line17_to_line19),L17L19),
   assertz(tr([[l17,[[l18,L17L17_18],[l19,L17L19]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line18_to_line18_21),L18L18_21),
   assertz(tr([[l18,[[l21,L18L18_21]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line19_to_line19),L19L19),
   fsa_regex_compile(file(line19_to_line20),L19L20),
   assertz(tr([[l19,[[l19,L19L19],[l20,L19L20]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line20_to_line20_21),L20L20_21),
   assertz(tr([[l20,[[l21,L20L20_21]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line21_to_line21_22),L21L21_22),
   assertz(tr([[l21,[[l22,L21L21_22]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line22_to_line22_09),L22L22_9),
   assertz(tr([[l22,[[l9,L22L22_9]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line23_to_line23_24),L23L23_24),
   assertz(tr([[l23,[[l24,L23L23_24]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line24_to_line24_25),L24L24_25),
   assertz(tr([[l24,[[l25,L24L24_25]]]|TrL])).

:- retract(tr(TrL)),
   fsa_regex_compile(file(line25_to_line25_09),L25L25_9),
   assertz(tr([[l25,[[l9,L25L25_9]]]|TrL])).

:- retract(tr(TrL)),
   assertz(tr([[l26,[]]|TrL])).

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

:- sort([!,#,/,'|','_f','_t',n,s,su,e,u,m_f,m_t,n_f,n_t,
         l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24,l25,l26,
         lte,gt,  
         x,p,y,yn,t,
         ch,nch],S),assertz(sigma(S)).

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
