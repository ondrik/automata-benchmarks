%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('/usr/local/src/fsa6/yap/fsa_library').
:- use_module(library(lists)).
:- use_module(library(apply_macros)).

#include "asmc.pl"

#ifdef TrClosure
:- fsa_global_set(pred_module,fsa_frozen).
#endif


%%%%%%%%%%%%%%%%%%%%%%%%

:- sort([< / <,< / >,< /e,< /i,< /il,< /ilx,< /ilxy,< /ily,< /ix,< /ixy,< /iy,< /o,< /ol,< /olx,< /olxy,< /oly,< /ox,< /oxy,< /oy,< /q0,< /q1,< /q2,< /q3,< /q4,< /q5,< /q6,< /q7,< /'|',> / <,> / >,> /e,> /i,> /il,> /ilx,> /ilxy,> /ily,> /ix,> /ixy,> /iy,> /o,> /ol,> /olx,> /olxy,> /oly,> /ox,> /oxy,> /oy,> /q0,> /q1,> /q2,> /q3,> /q4,> /q5,> /q6,> /q7,> /'|',e/ <,e/ >,e/e,e/i,e/il,e/ilx,e/ilxy,e/ily,e/ix,e/ixy,e/iy,e/o,e/ol,e/olx,e/olxy,e/oly,e/ox,e/oxy,e/oy,e/q0,e/q1,e/q2,e/q3,e/q4,e/q5,e/q6,e/q7,e/'|',i/ <,i/ >,i/e,i/i,i/il,i/ilx,i/ilxy,i/ily,i/ix,i/ixy,i/iy,i/o,i/ol,i/olx,i/olxy,i/oly,i/ox,i/oxy,i/oy,i/q0,i/q1,i/q2,i/q3,i/q4,i/q5,i/q6,i/q7,i/'|',il/ <,il/ >,il/e,il/i,il/il,il/ilx,il/ilxy,il/ily,il/ix,il/ixy,il/iy,il/o,il/ol,il/olx,il/olxy,il/oly,il/ox,il/oxy,il/oy,il/q0,il/q1,il/q2,il/q3,il/q4,il/q5,il/q6,il/q7,il/'|',ilx/ <,ilx/ >,ilx/e,ilx/i,ilx/il,ilx/ilx,ilx/ilxy,ilx/ily,ilx/ix,ilx/ixy,ilx/iy,ilx/o,ilx/ol,ilx/olx,ilx/olxy,ilx/oly,ilx/ox,ilx/oxy,ilx/oy,ilx/q0,ilx/q1,ilx/q2,ilx/q3,ilx/q4,ilx/q5,ilx/q6,ilx/q7,ilx/'|',ilxy/ <,ilxy/ >,ilxy/e,ilxy/i,ilxy/il,ilxy/ilx,ilxy/ilxy,ilxy/ily,ilxy/ix,ilxy/ixy,ilxy/iy,ilxy/o,ilxy/ol,ilxy/olx,ilxy/olxy,ilxy/oly,ilxy/ox,ilxy/oxy,ilxy/oy,ilxy/q0,ilxy/q1,ilxy/q2,ilxy/q3,ilxy/q4,ilxy/q5,ilxy/q6,ilxy/q7,ilxy/'|',ily/ <,ily/ >,ily/e,ily/i,ily/il,ily/ilx,ily/ilxy,ily/ily,ily/ix,ily/ixy,ily/iy,ily/o,ily/ol,ily/olx,ily/olxy,ily/oly,ily/ox,ily/oxy,ily/oy,ily/q0,ily/q1,ily/q2,ily/q3,ily/q4,ily/q5,ily/q6,ily/q7,ily/'|',ix/ <,ix/ >,ix/e,ix/i,ix/il,ix/ilx,ix/ilxy,ix/ily,ix/ix,ix/ixy,ix/iy,ix/o,ix/ol,ix/olx,ix/olxy,ix/oly,ix/ox,ix/oxy,ix/oy,ix/q0,ix/q1,ix/q2,ix/q3,ix/q4,ix/q5,ix/q6,ix/q7,ix/'|',ixy/ <,ixy/ >,ixy/e,ixy/i,ixy/il,ixy/ilx,ixy/ilxy,ixy/ily,ixy/ix,ixy/ixy,ixy/iy,ixy/o,ixy/ol,ixy/olx,ixy/olxy,ixy/oly,ixy/ox,ixy/oxy,ixy/oy,ixy/q0,ixy/q1,ixy/q2,ixy/q3,ixy/q4,ixy/q5,ixy/q6,ixy/q7,ixy/'|',iy/ <,iy/ >,iy/e,iy/i,iy/il,iy/ilx,iy/ilxy,iy/ily,iy/ix,iy/ixy,iy/iy,iy/o,iy/ol,iy/olx,iy/olxy,iy/oly,iy/ox,iy/oxy,iy/oy,iy/q0,iy/q1,iy/q2,iy/q3,iy/q4,iy/q5,iy/q6,iy/q7,iy/'|',o/ <,o/ >,o/e,o/i,o/il,o/ilx,o/ilxy,o/ily,o/ix,o/ixy,o/iy,o/o,o/ol,o/olx,o/olxy,o/oly,o/ox,o/oxy,o/oy,o/q0,o/q1,o/q2,o/q3,o/q4,o/q5,o/q6,o/q7,o/'|',ol/ <,ol/ >,ol/e,ol/i,ol/il,ol/ilx,ol/ilxy,ol/ily,ol/ix,ol/ixy,ol/iy,ol/o,ol/ol,ol/olx,ol/olxy,ol/oly,ol/ox,ol/oxy,ol/oy,ol/q0,ol/q1,ol/q2,ol/q3,ol/q4,ol/q5,ol/q6,ol/q7,ol/'|',olx/ <,olx/ >,olx/e,olx/i,olx/il,olx/ilx,olx/ilxy,olx/ily,olx/ix,olx/ixy,olx/iy,olx/o,olx/ol,olx/olx,olx/olxy,olx/oly,olx/ox,olx/oxy,olx/oy,olx/q0,olx/q1,olx/q2,olx/q3,olx/q4,olx/q5,olx/q6,olx/q7,olx/'|',olxy/ <,olxy/ >,olxy/e,olxy/i,olxy/il,olxy/ilx,olxy/ilxy,olxy/ily,olxy/ix,olxy/ixy,olxy/iy,olxy/o,olxy/ol,olxy/olx,olxy/olxy,olxy/oly,olxy/ox,olxy/oxy,olxy/oy,olxy/q0,olxy/q1,olxy/q2,olxy/q3,olxy/q4,olxy/q5,olxy/q6,olxy/q7,olxy/'|',oly/ <,oly/ >,oly/e,oly/i,oly/il,oly/ilx,oly/ilxy,oly/ily,oly/ix,oly/ixy,oly/iy,oly/o,oly/ol,oly/olx,oly/olxy,oly/oly,oly/ox,oly/oxy,oly/oy,oly/q0,oly/q1,oly/q2,oly/q3,oly/q4,oly/q5,oly/q6,oly/q7,oly/'|',ox/ <,ox/ >,ox/e,ox/i,ox/il,ox/ilx,ox/ilxy,ox/ily,ox/ix,ox/ixy,ox/iy,ox/o,ox/ol,ox/olx,ox/olxy,ox/oly,ox/ox,ox/oxy,ox/oy,ox/q0,ox/q1,ox/q2,ox/q3,ox/q4,ox/q5,ox/q6,ox/q7,ox/'|',oxy/ <,oxy/ >,oxy/e,oxy/i,oxy/il,oxy/ilx,oxy/ilxy,oxy/ily,oxy/ix,oxy/ixy,oxy/iy,oxy/o,oxy/ol,oxy/olx,oxy/olxy,oxy/oly,oxy/ox,oxy/oxy,oxy/oy,oxy/q0,oxy/q1,oxy/q2,oxy/q3,oxy/q4,oxy/q5,oxy/q6,oxy/q7,oxy/'|',oy/ <,oy/ >,oy/e,oy/i,oy/il,oy/ilx,oy/ilxy,oy/ily,oy/ix,oy/ixy,oy/iy,oy/o,oy/ol,oy/olx,oy/olxy,oy/oly,oy/ox,oy/oxy,oy/oy,oy/q0,oy/q1,oy/q2,oy/q3,oy/q4,oy/q5,oy/q6,oy/q7,oy/'|',q0/ <,q0/ >,q0/e,q0/i,q0/il,q0/ilx,q0/ilxy,q0/ily,q0/ix,q0/ixy,q0/iy,q0/o,q0/ol,q0/olx,q0/olxy,q0/oly,q0/ox,q0/oxy,q0/oy,q0/q0,q0/q1,q0/q2,q0/q3,q0/q4,q0/q5,q0/q6,q0/q7,q0/'|',q1/ <,q1/ >,q1/e,q1/i,q1/il,q1/ilx,q1/ilxy,q1/ily,q1/ix,q1/ixy,q1/iy,q1/o,q1/ol,q1/olx,q1/olxy,q1/oly,q1/ox,q1/oxy,q1/oy,q1/q0,q1/q1,q1/q2,q1/q3,q1/q4,q1/q5,q1/q6,q1/q7,q1/'|',q2/ <,q2/ >,q2/e,q2/i,q2/il,q2/ilx,q2/ilxy,q2/ily,q2/ix,q2/ixy,q2/iy,q2/o,q2/ol,q2/olx,q2/olxy,q2/oly,q2/ox,q2/oxy,q2/oy,q2/q0,q2/q1,q2/q2,q2/q3,q2/q4,q2/q5,q2/q6,q2/q7,q2/'|',q3/ <,q3/ >,q3/e,q3/i,q3/il,q3/ilx,q3/ilxy,q3/ily,q3/ix,q3/ixy,q3/iy,q3/o,q3/ol,q3/olx,q3/olxy,q3/oly,q3/ox,q3/oxy,q3/oy,q3/q0,q3/q1,q3/q2,q3/q3,q3/q4,q3/q5,q3/q6,q3/q7,q3/'|',q4/ <,q4/ >,q4/e,q4/i,q4/il,q4/ilx,q4/ilxy,q4/ily,q4/ix,q4/ixy,q4/iy,q4/o,q4/ol,q4/olx,q4/olxy,q4/oly,q4/ox,q4/oxy,q4/oy,q4/q0,q4/q1,q4/q2,q4/q3,q4/q4,q4/q5,q4/q6,q4/q7,q4/'|',q5/ <,q5/ >,q5/e,q5/i,q5/il,q5/ilx,q5/ilxy,q5/ily,q5/ix,q5/ixy,q5/iy,q5/o,q5/ol,q5/olx,q5/olxy,q5/oly,q5/ox,q5/oxy,q5/oy,q5/q0,q5/q1,q5/q2,q5/q3,q5/q4,q5/q5,q5/q6,q5/q7,q5/'|',q6/ <,q6/ >,q6/e,q6/i,q6/il,q6/ilx,q6/ilxy,q6/ily,q6/ix,q6/ixy,q6/iy,q6/o,q6/ol,q6/olx,q6/olxy,q6/oly,q6/ox,q6/oxy,q6/oy,q6/q0,q6/q1,q6/q2,q6/q3,q6/q4,q6/q5,q6/q6,q6/q7,q6/'|',q7/ <,q7/ >,q7/e,q7/i,q7/il,q7/ilx,q7/ilxy,q7/ily,q7/ix,q7/ixy,q7/iy,q7/o,q7/ol,q7/olx,q7/olxy,q7/oly,q7/ox,q7/oxy,q7/oy,q7/q0,q7/q1,q7/q2,q7/q3,q7/q4,q7/q5,q7/q6,q7/q7,q7/'|','|'/ <,'|'/ >,'|'/e,'|'/i,'|'/il,'|'/ilx,'|'/ilxy,'|'/ily,'|'/ix,'|'/ixy,'|'/iy,'|'/o,'|'/ol,'|'/olx,'|'/olxy,'|'/oly,'|'/ox,'|'/oxy,'|'/oy,'|'/q0,'|'/q1,'|'/q2,'|'/q3,'|'/q4,'|'/q5,'|'/q6,'|'/q7,'|'/'|'],S),assertz(sigma(S)).

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

