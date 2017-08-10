%%%%%%%%%%%%%%%%%%%%%%%%

%% :- consult('/usr/local/src/fsa6/swi/fsa_library').
:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the abstr_collprice of a slower run...

%% :- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(init(Init)).

%%......................

:- fsa_regex_compile(file(trs),TR),assertz(tr(TR)).

%%......................

:- fsa_regex_compile(file(bad),BAD),assertz(bad(BAD)).

:- fsa_regex_compile(file(good_result),GOOD),assertz(good(GOOD)).

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

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr1_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr2_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr3_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr4_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr5_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr6_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr7_g),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prS_g),P),assertz(pred_lang([P|PL])).

%% Taken not individually but from tr

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_g),P),assertz(pred_lang([P|PL])).

#endif

%%%%%%%%%%%%%%%

#ifdef PrActions

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr1_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr2_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr3_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr4_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr5_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr6_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr7_a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prS_a),P),assertz(pred_lang([P|PL])).

%% Taken not individually but from tr

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_a),P),assertz(pred_lang([P|PL])).

#endif

%%%%%%%%%%%%%%%

#ifdef PrSpecial

% :- assertz(pred_lang([])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr_slash),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pr_not_slash),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_x),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_y),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_bar),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_bang),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_sharp),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l1),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l2),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l3),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l4),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l5),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l6),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l7),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_l8),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_f),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_t),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_n),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_e),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_u),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pr_s),P),assertz(pred_lang([P|PL])).

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

:- sort(['/','!','#','_f','_t',n,e,s,u,'|',l,x,y,'l1','l2','l3','l4','l5','l6','l7','l8'],S),assertz(sigma(S)).

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
