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

%% :- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(bad(Init)).

%%......................

%%% No Deadlock-stuttering is needed---no deadlock is possible here!
%%% {b,c,w}* & ~minimize(domain(file(tr)))

%%% Choose either the relation with or without stuttering...

:- fsa_regex_compile(file('trs-iliv'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('tr-iliv'),TR),assertz(itr(TR)).

%%......................

:- fsa_regex_compile(file('bad-iloop'),BAD),assertz(init(BAD)).

%%......................

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),init(Init),assertz(pred_lang([Init,Good])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),init(Init),assertz(pred_lang([[Init,[0]],[Good,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),
%%    Good=fa(_,_,_,GoodFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Good,GoodFL]])).

%% :- bad(Bad),init(Init),assertz(pred_lang([Init,Bad])).

%% :- bad(Bad),init(Init),assertz(pred_lang([[Init,[0]],[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,BadFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Bad,BadFL]])).

#ifdef PrIni
:- init(Init),assertz(pred_lang([Init])).
#endif

%% :- init(Init),assertz(pred_lang([[Init,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([Good])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([[Good,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),Good=fa(_,_,_,GoodFL,_,_),assertz(pred_lang([[Good,GoodFL]])).
   
#ifdef PrBad
:- bad(Bad),assertz(pred_lang([Bad])).
#endif

%% :- bad(Bad),assertz(pred_lang([[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,FL,_,_),assertz(pred_lang([[Bad,FL]])).

%%......................

#ifdef PrGuards

:- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr1'),P),assertz(pred_lang([[P,[0]]|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr2'),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr3'),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr1-iloop'),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr2-iloop'),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr3-iloop'),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('pred-start-iloop'),P),assertz(pred_lang([P|PL])).


%%%%%

% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr1-a'),P),assertz(pred_lang([[P,[0]]|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr2-a'),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr3-a'),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr1-iloop-a'),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr2-iloop-a'),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr3-iloop-a'),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-start-iloop-a'),P),assertz(pred_lang([P|PL])).

#endif

%%......................

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred2),P),assertz(pred_lang([[P,[0]]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred3),P),assertz(pred_lang([[P,[0]]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred4),P),assertz(pred_lang([[P,[0]]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr1-iloop'),P),assertz(pred_lang([[P,[0]]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr2-iloop'),P),assertz(pred_lang([[P,[0]]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr3-iloop'),P),assertz(pred_lang([[P,[0]]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-start-iloop'),P),assertz(pred_lang([[P,[0]]|PL])).

%%......................

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred2),P),P=fa(_,_,_,FL,_,_),assertz(pred_lang([[P,FL]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred3),P),P=fa(_,_,_,FL,_,_),assertz(pred_lang([[P,FL]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred4),P),P=fa(_,_,_,FL,_,_),assertz(pred_lang([[P,FL]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr1-iloop'),P),P=fa(_,_,_,FL,_,_),assertz(pred_lang([[P,FL]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr2-iloop'),P),P=fa(_,_,_,FL,_,_),assertz(pred_lang([[P,FL]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-tr3-iloop'),P),P=fa(_,_,_,FL,_,_),assertz(pred_lang([[P,FL]|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('pred-start-iloop'),P),P=fa(_,_,_,FL,_,_),assertz(pred_lang([[P,FL]|PL])).

%%......................

%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(2).
max_for_appl_tuples(10).

%%......................

%% Abstraction based on forward collapsing:

#ifdef FwColl
abstract(State,AbsState) :- abstr_fw(State,AbsState).
#endif

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

:- sort([b,w,c,b0b,b0w,b0c,b0xb,b0xw,b1b,b1w,b1c,b1xb,b1xw,w0b,w0w,w0c,w0xb,w0xw,w1b,w1w,w1c,w1xb,w1xw,c0b,c0w,c0c,c0xb,c0xw,c1b,c1w,c1c,c1xb,c1xw,xb0b,xb0w,xb0c,xb0xb,xb0xw,xb1b,xb1w,xb1c,xb1xb,xb1xw,xw0b,xw0w,xw0c,xw0xb,xw0xw,xw1b,xw1w,xw1c,xw1xb,xw1xw],S),assertz(sigma(S)).

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
