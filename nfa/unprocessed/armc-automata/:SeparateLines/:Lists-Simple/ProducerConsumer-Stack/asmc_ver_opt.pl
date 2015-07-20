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

:- fsa_regex_compile(cleanup(file(tr_p1_0)),P1_0),
     assertz(tr(tr_p1_0,P1_0)),
     fsa_regex_compile(inverse(fa(P1_0)),IP1_0),
     assertz(tr(itr_p1_0,IP1_0)),
   fsa_regex_compile(cleanup(file(tr_p2_0)),P2_0),
     assertz(tr(tr_p2_0,P2_0)),
     fsa_regex_compile(inverse(fa(P2_0)),IP2_0),
     assertz(tr(itr_p2_0,IP2_0)),
   fsa_regex_compile(cleanup(file(tr_p2b_0)),P2b_0),
     assertz(tr(tr_p2b_0,P2b_0)),
     fsa_regex_compile(inverse(fa(P2b_0)),IP2b_0),
     assertz(tr(itr_p2b_0,IP2b_0)),
   fsa_regex_compile(cleanup(file(tr_p3_0)),P3_0),
     assertz(tr(tr_p3_0,P3_0)),
     fsa_regex_compile(inverse(fa(P3_0)),IP3_0),
     assertz(tr(itr_p3_0,IP3_0)),
   fsa_regex_compile(cleanup(file(tr_p4_0)),P4_0),
     assertz(tr(tr_p4_0,P4_0)),
     fsa_regex_compile(inverse(fa(P4_0)),IP4_0),
     assertz(tr(itr_p4_0,IP4_0)),
   fsa_regex_compile(cleanup(file(tr_p5_0)),P5_0),
     assertz(tr(tr_p5_0,P5_0)),
     fsa_regex_compile(inverse(fa(P5_0)),IP5_0),
     assertz(tr(itr_p5_0,IP5_0)),
   fsa_regex_compile(cleanup(file(tr_p6_0)),P6_0),
     assertz(tr(tr_p6_0,P6_0)),
     fsa_regex_compile(inverse(fa(P6_0)),IP6_0),
     assertz(tr(itr_p6_0,IP6_0)),
     
   fsa_regex_compile(cleanup(file(tr_p1_1)),P1_1),
     assertz(tr(tr_p1_1,P1_1)),
     fsa_regex_compile(inverse(fa(P1_1)),IP1_1),
     assertz(tr(itr_p1_1,IP1_1)),
   fsa_regex_compile(cleanup(file(tr_p2_1)),P2_1),
     assertz(tr(tr_p2_1,P2_1)),
     fsa_regex_compile(inverse(fa(P2_1)),IP2_1),
     assertz(tr(itr_p2_1,IP2_1)),
   fsa_regex_compile(cleanup(file(tr_p2b_1)),P2b_1),
     assertz(tr(tr_p2b_1,P2b_1)),
     fsa_regex_compile(inverse(fa(P2b_1)),IP2b_1),
     assertz(tr(itr_p2b_1,IP2b_1)),
   fsa_regex_compile(cleanup(file(tr_p3_1)),P3_1),
     assertz(tr(tr_p3_1,P3_1)),
     fsa_regex_compile(inverse(fa(P3_1)),IP3_1),
     assertz(tr(itr_p3_1,IP3_1)),
   fsa_regex_compile(cleanup(file(tr_p3s_1)),P3s_1),
     assertz(tr(tr_p3s_1,P3s_1)),
     fsa_regex_compile(inverse(fa(P3s_1)),IP3s_1),
     assertz(tr(itr_p3s_1,IP3s_1)),
   fsa_regex_compile(cleanup(file(tr_p4_1)),P4_1),
     assertz(tr(tr_p4_1,P4_1)),
     fsa_regex_compile(inverse(fa(P4_1)),IP4_1),
     assertz(tr(itr_p4_1,IP4_1)),
   fsa_regex_compile(cleanup(file(tr_p5_1)),P5_1),
     assertz(tr(tr_p5_1,P5_1)),
     fsa_regex_compile(inverse(fa(P5_1)),IP5_1),
     assertz(tr(itr_p5_1,IP5_1)),
   fsa_regex_compile(cleanup(file(tr_p6_1)),P6_1),
     assertz(tr(tr_p6_1,P6_1)),
     fsa_regex_compile(inverse(fa(P6_1)),IP6_1),
     assertz(tr(itr_p6_1,IP6_1)),
     
   fsa_regex_compile(cleanup(file(tr_c1_0)),C1_0),
     assertz(tr(tr_c1_0,C1_0)),
     fsa_regex_compile(inverse(fa(C1_0)),IC1_0),
     assertz(tr(itr_c1_0,IC1_0)),
   fsa_regex_compile(cleanup(file(tr_c2_0)),C2_0),
     assertz(tr(tr_c2_0,C2_0)),
     fsa_regex_compile(inverse(fa(C2_0)),IC2_0),
     assertz(tr(itr_c2_0,IC2_0)),
   fsa_regex_compile(cleanup(file(tr_c3_0)),C3_0),
     assertz(tr(tr_c3_0,C3_0)),
     fsa_regex_compile(inverse(fa(C3_0)),IC3_0),
     assertz(tr(itr_c3_0,IC3_0)),
   fsa_regex_compile(cleanup(file(tr_c4_0)),C4_0),
     assertz(tr(tr_c4_0,C4_0)),
     fsa_regex_compile(inverse(fa(C4_0)),IC4_0),
     assertz(tr(itr_c4_0,IC4_0)),
   fsa_regex_compile(cleanup(file(tr_c5_0)),C5_0),
     assertz(tr(tr_c5_0,C5_0)),
     fsa_regex_compile(inverse(fa(C5_0)),IC5_0),
     assertz(tr(itr_c5_0,IC5_0)),
   fsa_regex_compile(cleanup(file(tr_c6_0)),C6_0),
     assertz(tr(tr_c6_0,C6_0)),
     fsa_regex_compile(inverse(fa(C6_0)),IC6_0),
     assertz(tr(itr_c6_0,IC6_0)),
   fsa_regex_compile(cleanup(file(tr_c6d_0)),C6d_0),
     assertz(tr(tr_c6d_0,C6d_0)),
     fsa_regex_compile(inverse(fa(C6d_0)),IC6d_0),
     assertz(tr(itr_c6d_0,IC6d_0)),
   fsa_regex_compile(cleanup(file(tr_c7_0)),C7_0),
     assertz(tr(tr_c7_0,C7_0)),
     fsa_regex_compile(inverse(fa(C7_0)),IC7_0),
     assertz(tr(itr_c7_0,IC7_0)),

   fsa_regex_compile(cleanup(file(tr_c1_1)),C1_1),
     assertz(tr(tr_c1_1,C1_1)),
     fsa_regex_compile(inverse(fa(C1_1)),IC1_1),
     assertz(tr(itr_c1_1,IC1_1)),
   fsa_regex_compile(cleanup(file(tr_c2_1)),C2_1),
     assertz(tr(tr_c2_1,C2_1)),
     fsa_regex_compile(inverse(fa(C2_1)),IC2_1),
     assertz(tr(itr_c2_1,IC2_1)),
   fsa_regex_compile(cleanup(file(tr_c3_1)),C3_1),
     assertz(tr(tr_c3_1,C3_1)),
     fsa_regex_compile(inverse(fa(C3_1)),IC3_1),
     assertz(tr(itr_c3_1,IC3_1)),
   fsa_regex_compile(cleanup(file(tr_c4_1)),C4_1),
     assertz(tr(tr_c4_1,C4_1)),
     fsa_regex_compile(inverse(fa(C4_1)),IC4_1),
     assertz(tr(itr_c4_1,IC4_1)),
   fsa_regex_compile(cleanup(file(tr_c5_1)),C5_1),
     assertz(tr(tr_c5_1,C5_1)),
     fsa_regex_compile(inverse(fa(C5_1)),IC5_1),
     assertz(tr(itr_c5_1,IC5_1)),
   fsa_regex_compile(cleanup(file(tr_c6_1)),C6_1),
     assertz(tr(tr_c6_1,C6_1)),
     fsa_regex_compile(inverse(fa(C6_1)),IC6_1),
     assertz(tr(itr_c6_1,IC6_1)),
   fsa_regex_compile(cleanup(file(tr_c6d_1)),C6d_1),
     assertz(tr(tr_c6d_1,C6d_1)),
     fsa_regex_compile(inverse(fa(C6d_1)),IC6d_1),
     assertz(tr(itr_c6d_1,IC6d_1)),
   fsa_regex_compile(cleanup(file(tr_c7_1)),C7_1),
     assertz(tr(tr_c7_1,C7_1)),
     fsa_regex_compile(inverse(fa(C7_1)),IC7_1),
     assertz(tr(itr_c7_1,IC7_1)),

   fsa_regex_compile(cleanup(file(tr_check_cnt)),Check),
     assertz(tr(tr_check,Check)),
     fsa_regex_compile(inverse(fa(Check)),InvCheck),
     assertz(tr(itr_check,InvCheck)),

   TrToL=[
          [p2,tr_p1_1,itr_p1_1],
          [p2b,tr_p2_1,itr_p2_1],
          [p3,tr_p2b_1,itr_p2b_1],
	  [p4,tr_p3_1,itr_p3_1],
           [p3,tr_p3s_1,itr_p3s_1],
	  [p5,tr_p4_1,itr_p4_1],
	  [p6,tr_p5_1,itr_p5_1],
	  [b1,tr_p6_1,itr_p6_1],

          [e,tr_p1_0,itr_p1_0],
	  [e,tr_p2_0,itr_p2_0],
	  [e,tr_p2b_0,itr_p2b_0],
	  [e,tr_p3_0,itr_p3_0],
	  [e,tr_p4_0,itr_p4_0],
	  [e,tr_p5_0,itr_p5_0],
	  [e,tr_p6_0,itr_p6_0],

          [c2,tr_c1_1,itr_c1_1],
          [c3,tr_c2_1,itr_c2_1],
	  [c4,tr_c3_1,itr_c3_1],
	  [c5,tr_c4_1,itr_c4_1],
	  [c6,tr_c5_1,itr_c5_1],
	  [c7,tr_c6_1,itr_c6_1],
           [c6,tr_c6d_1,itr_c6d_1],
	  [b1,tr_c7_1,itr_c7_1],

          [e,tr_c1_0,itr_c1_0],
	  [e,tr_c2_0,itr_c2_0],
	  [e,tr_c3_0,itr_c3_0],
	  [e,tr_c4_0,itr_c4_0],
	  [e,tr_c5_0,itr_c5_0],
	  [e,tr_c6_0,itr_c6_0],
           [e,tr_c6d_0,itr_c6d_0],
	  [e,tr_c7_0,itr_c7_0]
	  
	 ],
	 
   TrL=[[b1,TrToL],
        [p2,TrToL],[p2b,TrToL],[p3,TrToL],[p4,TrToL],[p5,TrToL],[p6,TrToL],
        [c2,TrToL],[c3,TrToL],[c4,TrToL],[c5,TrToL],[c6,TrToL],[c7,TrToL],
	[e,[[e,tr_check,itr_check]]]],

   assertz(tr_itr(TrL)).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(init(b1,Init)).

%%......................

:- fsa_regex_compile(file(bad),BAD),assertz(bad(BAD)).

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

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(p_pr1_g),P),assertz(pred_lang([P|PL])).

%% Taken not individually but from tr

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(p_pr_g),P),assertz(pred_lang([P|PL])).

#endif

%%%%%%%%%%%%%%%

#ifdef PrActions

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(c_pr1_a),P),assertz(pred_lang([P|PL])).

%% Taken not individually but from tr

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(c_pr_a),P),assertz(pred_lang([P|PL])).

#endif

%%%%%%%%%%%%%%%

#ifdef PrSpecial


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

#ifdef FtColl
abstract(State,AbsState) :- abstr_ft(State,AbsState).  %% Choose this or ^^^
#endif

#ifdef FbColl
abstract(State,AbsState) :- abstr_fb(State,AbsState).	%% Choose this or vvv
#endif

#ifdef FbtColl
abstract(State,AbsState) :- abstr_ftb(State,AbsState).  %% Choose this or ^^^
#endif

#ifdef FlRefByFtColl
abstract(State,AbsState) :- abstr_fl_refby_ft(State,AbsState).  %% Choose this or ^^^
#endif

:- sort([!,#,/,'|','_f','_t',n,s,su,e,u,d,c,l,t,x,lock,unlock,
         p1,p2,p2b,p3,p4,p5,p6,c1,c2,c3,c4,c5,c6,c7,'00','01','10','11'],S),assertz(sigma(S)).

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

% Automatically detecting loops of length (max_collapsable_seq)//(unw_times)
%  -- consisting of data only (/ is considered the same as data).

#ifdef AutoDetDtLoopsFrom
:- AutoDetDtLoopsFrom=[StartUnwTimes,DtSymL],
   sort(DtSymL,SortedDtSymL),
   assertz(dt_aut_loop_det_from(SortedDtSymL)),
   assertz(unwound_times(StartUnwTimes)).
   
abstract(State,AbsState) :- 
  dt_aut_loop_det_from(DtSymL),
  dt_aut_loop_det(DtSymL,State,AbsState).

#endif

%%%%%%%%%%%%%%%%%%%%%%%%
