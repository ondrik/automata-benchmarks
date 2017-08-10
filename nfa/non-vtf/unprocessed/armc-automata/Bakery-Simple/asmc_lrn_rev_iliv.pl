%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

%% :- fsa_global_set(hash_size,1000000).

%%% The following decreases the memory consumption for the price of a slower run...

:- fsa_global_set(determinize_preds_cache,off),fsa_global_set(cleanup_list_cache,off).

%%%%%%%%%%%%%%%%%%%%%%%%

:- sort([b,w,c,b0b,b0w,b0c,b0xb,b0xw,b1b,b1w,b1c,b1xb,b1xw,w0b,w0w,w0c,w0xb,w0xw,w1b,w1w,w1c,w1xb,w1xw,c0b,c0w,c0c,c0xb,c0xw,c1b,c1w,c1c,c1xb,c1xw,xb0b,xb0w,xb0c,xb0xb,xb0xw,xb1b,xb1w,xb1c,xb1xb,xb1xw,xw0b,xw0w,xw0c,xw0xb,xw0xw,xw1b,xw1w,xw1c,xw1xb,xw1xw],S),assertz(sigma(S)).

%%......................

:- fsa_regex_compile(file('bad-iloop'),Bad),assertz(init(Bad)).

%%......................

:- fsa_regex_compile(file('trs-iliv'),TR),fsa_regex_compile(inverse(fa(TR)),ITR),assertz(tr(ITR)).

%%......................

:- fsa_regex_compile(file(init),Init),assertz(bad(Init)).

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef LrnGenConfBad
:- bad(fa(_,QN,_,_,_,_)),assertz(gen_conf_up_to(QN)).
#endif

#ifdef LrnGenConfHalfBad
:- bad(fa(_,QN,_,_,_,_)),Lim is QN//2,assertz(gen_conf_up_to(Lim)).
#endif

#ifdef LrnGenConfTwiceBad
:- bad(fa(_,QN,_,_,_,_)),Lim is QN*2,assertz(gen_conf_up_to(Lim)).
#endif

%%......................

#ifdef LrnGenConfInit
:- init(fa(_,QN,_,_,_,_)),assertz(gen_conf_up_to(QN)).
#endif

#ifdef LrnGenConfHalfInit
:- init(fa(_,QN,_,_,_,_)),Lim is QN//2,assertz(gen_conf_up_to(Lim)).
#endif

#ifdef LrnGenConfTwiceInit
:- init(fa(_,QN,_,_,_,_)),Lim is QN*2,assertz(gen_conf_up_to(Lim)).
#endif

%%......................

#ifdef LrnGenConfOne
:- assertz(gen_conf_up_to(1)).
#endif

%%......................

#ifdef LrnGenConfSp
:- assertz(gen_conf_up_to(LrnGenConfSp)).
#endif

%%%%%%%%%%%%%%%%%%%%%%%%

#ifdef LrnIniCollHalfGen
:- gen_conf_up_to(N),Lim is N//2,assertz(coll_fin_lang_up_to(Lim)).
#endif

#ifdef LrnIniCollOne
:- assertz(coll_fin_lang_up_to(1)).
#endif

#ifdef LrnIniCollZero
:- assertz(coll_fin_lang_up_to(0)).
#endif

#ifdef LrnIniCollSp
:- assertz(coll_fin_lang_up_to(LrnIniCollSp)).
#endif

%%%%%%%%%%%%%%%%%%%%%%%%

