%% :- use_module('/usr/local/src/FSA/src-compiled-sicstus/fsa_library').
%% :- use_module(library(lists)).

%% :- consult('/usr/local/src/FSA/src-compiled-swi/fsa_library').

:- consult('/usr/local/src/FSA/src-compiled-yap/fsa_library').
:- use_module(library(lists)).

:- consult(asmc).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(init(Init)).

%%......................

:- fsa_regex_compile(file('trs-inv'),TR),assertz(tr(TR)).

%%......................

:- fsa_regex_compile(file(imux),MUX),assertz(good(MUX)).

%%......................

:- good(Good),init(Init),assertz(pred_lang([Init,Good])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred1),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred2a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred2b),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred3a),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred3b),P),assertz(pred_lang([P|PL])).  

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred4),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred5),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred6a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred6b),P),assertz(pred_lang([P|PL])).  

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred7),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred8),P),assertz(pred_lang([P|PL])).

%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(1).
max_for_appl_tuples(20).

%% Abstraction based on collapsing:

abstract(State,AbsState) :- abstr_coll(State,AbsState).

%% Testing emptiness of intersectins of abstractions and predicates:
%% Requires new_pred to be initially asserted (see below) and its use enabled in the sources.

%% abstract(State,AbsState) :- abstr_coll_debug(State,AbsState).

%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic with_self_loop_acclr/0, new_pred/1.

%% with_self_loop_acclr.

%% For producing a separate list of newly added predicates 
%% or debugging the collapsing method...

new_pred([]).

%%%%%%%%%%%%%%%%%%%%%%%%
