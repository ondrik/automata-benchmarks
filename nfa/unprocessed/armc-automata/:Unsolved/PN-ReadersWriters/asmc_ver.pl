%%%%%%%%%%%%%%%%%%%%%%%%

%% :- consult('/usr/local/src/FSA/src-compiled-yap/fsa_library').
%% :- consult('/usr/local/src/fsa6/yap/fsa_library').
%% :- consult('/homes/kazi/vojnar/Tools/fsa6/lib/yap/fsa_library').

:- consult('/usr/local/src/fsa6/yap/fsa_library').

:- use_module(library(lists)).
:- use_module(library(apply_macros)).

%% :- use_module(library(ordsets)),use_module(library(apply_macros)).

:- consult(asmc).

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the price of a slower run...

%% :- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

:- fsa_regex_compile(file(empty),Init),assertz(init(Init)).

%%......................

:- fsa_regex_compile(file(trs),TR),assertz(tr(TR)).

%%......................

:- fsa_regex_compile(file(nmux),BAD),assertz(bad(BAD)).

%%......................

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),init(Init),assertz(pred_lang([Init,Good])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),init(Init),assertz(pred_lang([[Init,[0]],[Good,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),
%%    Good=fa(_,_,_,GoodFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Good,GoodFL]])).

%% :- bad(Bad),init(Init),assertz(pred_lang([Init,Bad])).

%% :- bad(Bad),init(Init),assertz(pred_lang([[Init,[0]],[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,BadFL,_,_),init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL],[Bad,BadFL]])).

%% :- init(Init),assertz(pred_lang([Init])).

%% :- init(Init),assertz(pred_lang([[Init,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([Good])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([[Good,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),Good=fa(_,_,_,GoodFL,_,_),assertz(pred_lang([[Good,GoodFL]])).
   
:- bad(Bad),assertz(pred_lang([Bad])).

%% :- bad(Bad),assertz(pred_lang([[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,FL,_,_),assertz(pred_lang([[Bad,FL]])).

%%%%%%%%%

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('guard_t1'),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('guard_t2'),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('guard_t4'),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('guard_t5'),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('guard_t7'),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('guard_t9'),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('guard_t10'),P),assertz(pred_lang([P|PL])).

%%%%%%%%%

:- retract(pred_lang(PL)),fsa_regex_compile(file('inv_num_read'),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('inv_num_wr_wait'),P),assertz(pred_lang([P|PL])).

:- retract(pred_lang(PL)),fsa_regex_compile(file('inv_num_write'),P),assertz(pred_lang([P|PL])).

%%%%%%%%%

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('aaa-npr5'),P),assertz(pred_lang([P|PL])).

%% :- retract(pred_lang(PL)),fsa_regex_compile(file('bbb-npr5'),P),assertz(pred_lang([P|PL])).

%%%%%%%%%

%%% :- pred_lang(PL),assertz(cp_pred_lang(PL)).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%%......................

%% No abstraction.

%% abstract(State,State).

%%......................

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(2).
max_for_appl_tuples(10).

%%......................

%% Abstraction based on forward collapsing:

abstract(State,AbsState) :- abstr_fw(State,AbsState).

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

%% Testing emptiness of intersections of abstractions and predicates:
%% Requires new_pred to be initially asserted (see below) and its use enabled in the sources.

%% abstract(State,AbsState) :- abstr_fw_debug(State,AbsState).

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

%%......................

%% Abstraction based on languages with a bounded length of words:

%% abstract(State,AbsState) :- abstr_fl(State,AbsState).

:- sort([c,i,w,'00','01','10','11'],S),assertz(sigma(S)).

:- bad(fa(_,QN,_,_,_,_)),assertz(fin_lang_up_to(QN)).

%% :- bad(fa(_,QN,_,_,_,_)),Lim is QN//2,assertz(fin_lang_up_to(Lim)).

%% :- assertz(fin_lang_up_to(1)).

%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic with_self_loop_acclr/0, new_pred/1.

with_self_loop_acclr.

%% For producing a separate list of newly added predicates 
%% or debugging the collapsing method...

new_pred([]).

%%%%%%%%%%%%%%%%%%%%%%%%

