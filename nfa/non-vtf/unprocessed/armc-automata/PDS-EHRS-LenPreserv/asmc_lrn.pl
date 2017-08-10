%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

:- sort([#,p,u,e,m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,s0,s1,s2,s3,s4,s5,a0,a1,a2],S),assertz(sigma(S)).

%%......................

:- fsa_regex_compile(file(init),Init),assertz(init(Init)).

%%......................

:- fsa_regex_compile(file(trs),TR),assertz(tr(TR)).

%%......................

:- fsa_regex_compile(file(bad),Bad),assertz(bad(Bad)).

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

