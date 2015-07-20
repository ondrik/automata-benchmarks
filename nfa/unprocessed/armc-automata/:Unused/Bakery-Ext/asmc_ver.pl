%% :- use_module('/usr/local/src/FSA/src-compiled-sicstus/fsa_library').
%% :- use_module(library(lists)).

%% :- consult('/usr/local/src/FSA/src-compiled-swi/fsa_library').

:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).

:- consult(asmc).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(init),Init),assertz(init(Init)).

%%......................

:- fsa_regex_compile(file(trs),TR),assertz(tr(TR)).

%%......................

:- fsa_regex_compile(file(mux),MUX),assertz(good(MUX)).

%%......................

%% :- good(Good),init(Init),assertz(pred_lang([Init,Good])).

%% :- good(Good),init(Init),assertz(pred_lang([[Init,[0]],[Good,[0]]])).

%% :- good(Good),fsa_regex_compile(complement(fa(Good)),Bad),init(Init),assertz(pred_lang([Init,Bad])).

%% :- good(Good),fsa_regex_compile(complement(fa(Good)),Bad),init(Init),assertz(pred_lang([[Init,[0]],[Bad,[0]]])).

%% :- init(Init),assertz(pred_lang([Init])).

%% :- init(Init),assertz(pred_lang([[Init,[0]]])).

%% :- good(Good),assertz(pred_lang([Good])).

%% :- good(Good),assertz(pred_lang([[Good,[0]]])).

:- good(Good),fsa_regex_compile(complement(fa(Good)),Bad),assertz(pred_lang([Bad])).

%% :- good(Good),fsa_regex_compile(complement(fa(Good)),Bad),assertz(pred_lang([[Bad,[0]]])).

%%......................

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred1),P),assertz(pred_lang([P|PL])). 

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred2),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file(pred3),P),assertz(pred_lang([P|PL])). 

%%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(1).
max_for_appl_tuples(20).

%%......................

%% Abstraction based on forward collapsing:

abstract(State,AbsState) :- abstr_coll(State,AbsState).

%% Checking whether the abstraction is an overapproximation of the concrete state set.

%% abstract(State,AbsState) :-
%%   abstr_coll(State,AbsState),
%%   fsa_regex_compile(intersect(complement(fa(AbsState)),fa(State)),X),
%%   ( (empty(X), !,
%% 	write('Abstraction is an overapproximation!'),nl);
%%     (write('Abstraction is NOT an overapproximation!'),nl,
%% 	fsa_write_file(xxxState,State),
%% 	fsa_write_file(xxxAbsState,AbsState),
%% 	trace) ).

%% Testing emptiness of intersectins of abstractions and predicates:
%% Requires new_pred to be initially asserted (see below) and its use enabled in the sources.

%% abstract(State,AbsState) :- abstr_coll_debug(State,AbsState).

%%......................

%% Abstraction based on forward/backward collapsing:

%% abstract(State,AbsState) :- abstr_fwbw(State,AbsState).

%% Abstraction based on forward/backward collapsing - testing disjunctness of abstractions and bad sets

%% abstract(State,AbsState) :- abstr_fwbw_debug(State,AbsState).

%%......................

%% Abstraction based on backward collapsing:

%% abstract(State,AbsState) :- abstr_bw(State,AbsState).

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

%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic with_self_loop_acclr/0, new_pred/1.

%% with_self_loop_acclr.

%% For producing a separate list of newly added predicates 
%% or debugging the collapsing method...

new_pred([]).

%%%%%%%%%%%%%%%%%%%%%%%%
