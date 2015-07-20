%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

:- sort([a00,a01,a10,a11,a20,a21,b00,b01,b10,b11,b20,b21,c00,c01,c10,c11,c20,c21,d00,d01,d10,d11,d20,d21,e00,e01,e10,e11,e20,e21,f00,f01,f10,f11,f20,f21],S),assertz(sigma(S)).

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

