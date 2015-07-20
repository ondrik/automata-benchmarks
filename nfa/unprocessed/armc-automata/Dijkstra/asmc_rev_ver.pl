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

:- fsa_regex_compile(complement(file(mux)),NMUX),assertz(init(NMUX)).

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
   
#ifdef PrBaddit asmc_ver_iliv.pl &
[10] 21475
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ nedit asmc_rev_ver_iliv.pl &
[11] 21487
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ cp :run-fw-iliv :run-fw-iliv-NEW
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ cp :run-bw-iliv :run-bw-iliv-NEW
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ nedit :run-fw-iliv &
[12] 21584
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ nedit :run-bw-iliv &
[13] 21741
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ mv :RESULTS-FW :RESULTS-FW-LIV
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ mv :RESULTS-BW :RESULTS-BW-LIV
Press any key to continue...

:- bad(Bad),assertz(pred_lang([Bad])).
#endif

%% :- bad(Bad),assertz(pred_lang([[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,FL,_,_),assertz(pred_lang([[Bad,FL]])).

%%......................

#ifdef PrGuardsdit asmc_ver_iliv.pl &
[10] 21475
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ nedit asmc_rev_ver_iliv.pl &
[11] 21487
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ cp :run-fw-iliv :run-fw-iliv-NEW
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ cp :run-bw-iliv :run-bw-iliv-NEW
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ nedit :run-fw-iliv &
[12] 21584
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ nedit :run-bw-iliv &
[13] 21741
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ mv :RESULTS-FW :RESULTS-FW-LIV
Press any key to continue...
vojnar@marvin:~/MyExperiments/ASMC/Bakery-Simple$ mv :RESULTS-BW :RESULTS-BW-LIV
Press any key to continue...


:- retract(pred_lang(PL)),fsa_regex_compile(file(pred1),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred2a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred2b),P),assertz(pred_lang([P|PL])). 

%%% Encoding that somebody is at location b
%%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred2xxx),P),assertz(abs_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred3a),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred3b),P),assertz(pred_lang([P|PL])).  

%%% Encoding that somebody is at location c
%%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred3xxx),P),assertz(abs_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred4),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred5),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred6a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred6b),P),assertz(pred_lang([P|PL])).  

%%% Encoding that somebody is at location e
%%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred6xxx),P),assertz(abs_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred7),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(pred8),P),assertz(pred_lang([P|PL])).

%%%%%%%%%%%%%%%

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred1_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred2a_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred2b_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred3a_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred3b_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred4_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred5_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred6a_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred6b_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred7_a),P),assertz(pred_lang([P|PL])).

% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred8_a),P),assertz(pred_lang([P|PL])).

#endif

%%......................

%%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

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

%% Testing emptiness of intersectins of abstractions and predicates:
%% Requires new_pred to be initially asserted (see below) and its use enabled in the sources.

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

:- sort([a00,a01,a10,a11,a20,a21,b00,b01,b10,b11,b20,b21,c00,c01,c10,c11,c20,c21,d00,d01,d10,d11,d20,d21,e00,e01,e10,e11,e20,e21,f00,f01,f10,f11,f20,f21],S),assertz(sigma(S)).

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
