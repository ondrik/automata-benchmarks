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

%% Manually added to detect presence of a process at a (turns out to be unnecessary)
%% :- retract(pred_lang(PL)),fsa_regex_compile(file(ipred1xxx),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred2),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred3aa),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred3ab),P),assertz(pred_lang([P|PL])). 

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred3b),P),assertz(pred_lang([P|PL])). 

%% Manually added to detect presence of a process at c (this should, however, be clear from ^^^^)
%% :- retract(pred_lang(PL)),fsa_regex_compile(file(ipred3xxx),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred4a),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred4b),P),assertz(pred_lang([P|PL])).

%% Manually added to detect presence of a process at d (turns out to be unnecessary)
%% :- retract(pred_lang(PL)),fsa_regex_compile(file(ipred4xxx),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred5),P),assertz(pred_lang([P|PL])). 

%% Manually added to detect presence of a process at e
%% :- retract(pred_lang(PL)),fsa_regex_compile(file(ipred5xxx),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred6),P),assertz(pred_lang([P|PL])).

%% Manually added to detect presence of a process at f
%% :- retract(pred_lang(PL)),fsa_regex_compile(file(ipred6xxx),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file(ipred7),P),assertz(pred_lang([P|PL])).

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
