%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

%%%%%%%%%%%%%%%%%%%%%%%%

:- sort([<,>,e,i,il,ilx,ilxy,ily,ix,ixy,iy,o,ol,olx,olxy,oly,ox,oxy,oy,q0,q1,q2,q3,q4,q5,q6,q7,'|'],S),assertz(sigma(S)).

%%......................

:- fsa_regex_compile(file(not_rev_list),Bad),assertz(init(Bad)).

%%......................

:- fsa_regex_compile(file(trs),TR),fsa_regex_compile(inverse(fa(TR)),ITR),assertz(tr(ITR)).

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
