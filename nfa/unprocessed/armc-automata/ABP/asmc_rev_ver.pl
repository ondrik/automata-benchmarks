%%%%%%%%%%%%%%%%%%%%%%%%

%% :- consult('/usr/local/src/FSA/src-compiled-swi/fsa_library').

%% :- consult('/usr/local/src/FSA/src-compiled-yap/fsa_library').
:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(bad(Init)).

%%......................

:- fsa_regex_compile(file(trs),TR),assertz(itr(TR)).

%%......................

:- fsa_regex_compile(complement(file(safe)),NSAFE),assertz(init(NSAFE)).

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

:- retract(pred_lang(PL)),fsa_regex_compile(file(prl1),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prl2),P),assertz(pred_lang([P|PL])).

%%...

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prl1_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prl2_a),P),assertz(pred_lang([P|PL])).

%%...............

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr1),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr2),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr3),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr4),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr4x),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr5),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr6),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr7),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr8),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prr8x),P),assertz(pred_lang([P|PL])).

%%...

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr1_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr2_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr3_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr4_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr4x_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr5_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr6_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr7_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr8_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prr8x_a),P),assertz(pred_lang([P|PL])).

#ifdef IniInit
:- init(Init),Init=fa(_,QI,_,_,_,_),assertz(fin_lang_up_to(QI)).
#endif

#ifdef IniHalfInit
:- init(Init),Init=fa(_,QI,_,_,_,_),Lim is QI//2,assertz(fin_lang_up_to(Lim)).
#endif

%%...........

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs1),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs1x),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs2),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs3),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs4),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs5),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs5x),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs6),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs7),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(prs8),P),assertz(pred_lang([P|PL])).

%%...

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs1_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs1x_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs2_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs3_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs4_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs5_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs5x_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs6_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs7_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(prs8_a),P),assertz(pred_lang([P|PL])).

#endif

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
#ifdef IniInit
:- init(Init),Init=fa(_,QI,_,_,_,_),assertz(fin_lang_up_to(QI)).
#endif

#ifdef IniHalfInit
:- init(Init),Init=fa(_,QI,_,_,_,_),Lim is QI//2,assertz(fin_lang_up_to(Lim)).
#endif

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

:- sort([i,'0','1',a,b,c,d,e,f,g,h,t],S),assertz(sigma(S)).

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
