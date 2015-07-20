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

:- fsa_regex_compile(file(init),Init),assertz(init(Init)).

%%......................

:- fsa_regex_compile(file(trs),TR),assertz(tr(TR)).

%%......................

:- fsa_regex_compile(file(bad),BAD),assertz(bad(BAD)).

%% :- fsa_regex_compile(file(good_result),GOOD),assertz(good(GOOD)).

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef PrInitGood
:- good(Good),init(Init),assertz(pred_lang([Init,Good])).
#endif

%% :- good(Good),init(Init),assertz(pred_lang([[Init,[0]],[Good,[0]]])).

%% :- good(Good),Good=fa(_,_,_,GoodFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Good,GoodFL]])).

#ifdef PrInitBad
:- bad(Bad),init(Init),assertz(pred_lang([Init,Bad])).
#endif

%% :- bad(Bad),init(Init),assertz(pred_lang([[Init,[0]],[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,BadFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Bad,BadFL]])).

#ifdef PrInit
:- init(Init),assertz(pred_lang([Init])).
#endif

%% :- init(Init),assertz(pred_lang([[Init,[0]]])).

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
:- init(Init),good(Good),bad(Bad),assertz(pred_lang([Init,Good,Bad])).
#endif

%%......................

#ifdef PrGuards

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr01_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr02_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr03_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr04_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr05_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr06_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr07_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr08_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr09_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr10_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr11_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr12_g),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr13_g),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr14_g),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr15_g),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr16_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr17_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr18_g),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr19_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr20_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr21_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr22_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr23_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr24_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prS_g),P),assertz(pred_lang([P|PL])).

%% Taken not individually but from tr

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_g),P),assertz(pred_lang([P|PL])).

#endif

%%%%%%%%%%%%%%%

#ifdef PrActions

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr01_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr02_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr03_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr04_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr05_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr06_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr07_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr08_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr09_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr10_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr11_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr12_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr13_a),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr14_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr15_a),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr16_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr17_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr18_a),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr19_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr20_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr21_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr22_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr23_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr24_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prS_a),P),assertz(pred_lang([P|PL])).

%% Taken not individually but from tr

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_a),P),assertz(pred_lang([P|PL])).

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
         l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l15,l17,l18,l19,l20,l21,l22,l23,l24,l25,  %% l14, l16, l25n,
         %% lte,gt,  
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
:- init(Init),Init=fa(_,QI,_,_,_,_),assertz(fin_lang_up_to(QI)).
#endif

#ifdef IniHalfInit
:- init(Init),Init=fa(_,QI,_,_,_,_),Lim is QI//2,assertz(fin_lang_up_to(Lim)).
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

#ifdef RefWrtRepSym

:- assertz(rep_sym(RefWrtRepSym)).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%
