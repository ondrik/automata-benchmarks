%% :- use_module('/usr/local/src/FSA/src-compiled-sicstus/fsa_library').
%% :- use_module(library(lists)).

%% :- consult('/usr/local/src/FSA/src-compiled-swi/fsa_library').

%% :- consult('/usr/local/src/FSA/src-compiled-yap/fsa_library').
%% :- use_module(library(lists)).

:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).

:- consult(asmc).

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the price of a slower run...

%% :- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_regex_compile(file(init),Init),assertz(bad(Init)).

:- fsa_regex_compile(file(init2),Init),assertz(bad(Init)).

%%......................

%%% No Deadlock-stuttering is needed---no deadlock is possible here!
%%% {b,c,w}* & ~minimize(domain(file(tr)))

%%% Choose either the relation with or without stuttering...

%% :- fsa_regex_compile(file('trs-2lev'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('trs-2lev-min'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('trs-2lev-inv'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('trs-3lev'),TR),assertz(itr(TR)).

:- fsa_regex_compile(file('trs-3lev-min'),TR),assertz(itr(TR)).

%%...

%% :- fsa_regex_compile(file('trs-10lev'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('trs-10lev-min'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('trs-10lev-inv'),TR),assertz(itr(TR)).

%%...

%% :- fsa_regex_compile(file('trs-200lev'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('trs-200lev-min'),TR),assertz(itr(TR)).

%% :- fsa_regex_compile(file('trs-200lev-inv'),TR),assertz(itr(TR)).

%%......................

%% :- fsa_regex_compile(file('bad'),Bad),assertz(init(Bad)).

:- fsa_regex_compile(file('bad2'),Bad),assertz(init(Bad)).

%% :- fsa_regex_compile(file('safe2'),Good),fsa_regex_compile(complement(fa(Good)),Bad),assertz(init(Bad)).

%% :- fsa_regex_compile(file('safe2'),Good),fsa_regex_compile(file('inv2-2lev'),Inv2),
%%    fsa_regex_compile(intersection(complement(fa(Good)),fa(Inv2)),Bad),assertz(init(Bad)).

%% :- fsa_regex_compile(file('safe2'),Good),fsa_regex_compile(file('inv2-10lev'),Inv2),
%%    fsa_regex_compile(intersection(complement(fa(Good)),fa(Inv2)),Bad),assertz(init(Bad)).

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

%% :- init(Init),Init=fa(_,_,_,InitFL,_,_),assertz(pred_lang([[Init,InitFL]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([Good])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),assertz(pred_lang([[Good,[0]]])).

%% :- bad(Bad),fsa_regex_compile(complement(fa(Bad)),Good),Good=fa(_,_,_,GoodFL,_,_),assertz(pred_lang([[Good,GoodFL]])).
   
:- bad(Bad),assertz(pred_lang([Bad])).

%% :- bad(Bad),assertz(pred_lang([[Bad,[0]]])).

%% :- bad(Bad),Bad=fa(_,_,_,FL,_,_),assertz(pred_lang([[Bad,FL]])).

%%......................

%% :- assertz(pred_lang([])).

%% :- fsa_regex_atom_compile('{}',Empty),assertz(pred_lang([Empty])).

%%......................

%% :- fsa_regex_compile(file('inv1-2lev'),Inv1),assertz(pred_lang([Inv1])).

%% :- fsa_regex_compile(file('inv1-3lev'),Inv1),assertz(pred_lang([Inv1])).

%%......................

%% :- fsa_regex_compile(file('inv1-10lev'),Inv1),assertz(pred_lang([Inv1])).

%% :- fsa_regex_compile(file('inv1-10lev'),Inv1),fsa_regex_compile(complement(fa(Inv1)),NInv1),assertz(pred_lang([NInv1])).

%% :- fsa_regex_compile(file('inv1-10lev'),Inv1),bad(Bad),assertz(pred_lang([Bad,Inv1])).
   
%% :- fsa_regex_compile(file('inv1-10lev'),Inv1),fsa_regex_compile(complement(fa(Inv1)),NInv1),
%%    bad(Bad),assertz(pred_lang([Bad,NInv1])).

%% :- fsa_regex_compile(file('inv1-10lev'),Inv1),bad(Bad),init(Init),assertz(pred_lang([Init,Bad,Inv1])).

%% :- fsa_regex_compile(file('inv1-10lev'),Inv1),bad(Bad),fsa_regex_compile(union(complement(fa(Inv1)),fa(Bad)),BigBad),
%%    assertz(pred_lang([BigBad])).

%%......................

%% :- fsa_regex_compile(file('inv1-200lev'),Inv1),assertz(pred_lang([Inv1])).

%% :- fsa_regex_compile(file('inv1-200lev'),Inv1),bad(Bad),fsa_regex_compile(union(complement(fa(Inv1)),fa(Bad)),BigBad),
%%    assertz(pred_lang([BigBad])).

%%......................

%% :- fsa_regex_compile(file('inv1-10lev-01'),Inv1),assertz(pred_lang([Inv1])).

%%......................

%% :- fsa_regex_compile(file('inv2-10lev'),Inv2),assertz(pred_lang([Inv2])).

%% :- bad(Bad),fsa_regex_compile(file('inv2-10lev'),Inv2),assertz(pred_lang([Bad,Inv2])).

%% :- fsa_regex_compile(file('inv2-10lev'),Inv2),bad(Bad),fsa_regex_compile(union(complement(fa(Inv2)),fa(Bad)),BigBad),
%%    assertz(pred_lang([BigBad])).

%%%%%%%%%%%%%%%%%%%%%%%%

%% Choose some of the below abstraction methods:

%% Abstraction based on classical language operations:

%% abstract(State,AbsState) :- abstr_lang(State,AbsState).

max_tuple(2).
max_for_appl_tuples(10).

%%......................

%% Abstraction based on forward collapsing:

%% abstract(State,AbsState) :- abstr_coll(State,AbsState).

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

abstract(State,AbsState) :- abstr_bw(State,AbsState).

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

%% Just for debugging the collapsing method...

new_pred([]).

%%%%%%%%%%%%%%%%%%%%%%%%
