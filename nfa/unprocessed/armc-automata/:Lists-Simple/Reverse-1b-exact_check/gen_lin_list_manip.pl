:- consult('/usr/local/src/fsa6/yap/fsa_library').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Working with ordering of data items and pointer variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_v1v2([V1|_],V1,_) :- !.

is_v1v2([V2|_],_,V2) :- !, fail.

is_v1v2([_|ItemVarList],V1,V2) :- is_v1v2(ItemVarList,V1,V2).

%-------------------------------------------------------------------------------

ltv1([V1|_],V1,[]) :- !.

ltv1([X|ItemVarList1],V1,[X|ItemVarList2]) :- ltv1(ItemVarList1,V1,ItemVarList2).

%-------------------------------------------------------------------------------

gtv1([V1|ItemVarList],V1,ItemVarList) :- !.

gtv1([_|ItemVarList1],V1,ItemVarList2) :- gtv1(ItemVarList1,V1,ItemVarList2).

%-------------------------------------------------------------------------------

v1v2([V1|ItemVarList1],V1,V2,ItemVarList2) :- ltv1(ItemVarList1,V2,ItemVarList2), !.

v1v2([_|ItemVarList1],V1,V2,ItemVarList2) :- v1v2(ItemVarList1,V1,V2,ItemVarList2).

%-------------------------------------------------------------------------------

do_gen_comma_sep_list([]) :- !.

do_gen_comma_sep_list([X|List]) :- write(','), write(X), do_gen_comma_sep_list(List).

%...............................................................................

gen_comma_sep_list([]) :- !.

gen_comma_sep_list([X|List]) :- write(X), do_gen_comma_sep_list(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generating the shifting construction for v1->next := v2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_shift_letters([],_) :- !.

gen_shift_letters([A|ItemVarList],I) :-
  S2 is 9 + I*6 + 1,
  S3 is 9 + I*6 + 2,
  S4 is 9 + I*6 + 3,
  S5 is 9 + I*6 + 4,
  S7 is 9 + I*6 + 5,
  S9 is 9 + I*6 + 6,
  write('trans('),write(S2),write(',''_f''/''_f'',1),'), nl,
  write('trans('),write(S3),write(',[]/ '),write(A),write(',1),'), nl,
  write('trans('),write(S4),write(','),write(A),write(' /[],'),write(S2),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',[]/ '),write(A),write(','),write(S4),write('),'), nl,
  write('trans('),write(6),write(',''_t''/''_t'','),write(S5),write('),'), nl,
  write('trans('),write(6),write(','),write(A),write(' /[],'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(',''_f''/''_f'','),write(S9),write('),'), nl,
  write('trans('),write(S9),write(',''_t''/''_t'','),write(S3),write('),'), nl,
  write('trans('),write(S9),write(',$@(not_in([]))/ $@(not_in([])),'),write(S9),write('),'), nl,
  II is I+1,
  gen_shift_letters(ItemVarList,II).

%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_shifting(FOut,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  length(ItemVarList,Len),
  N is 10 + Len*6,
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write(N), write(', '), nl,
  write('[ '), nl,
  write('0'), nl,
  write('], '), nl,
  write('[ '), nl,
  write('1'), nl,
  write('], '), nl,
  write('[ '), nl,
  gen_shift_letters(ItemVarList,0),
  write('trans(0,s/s,8),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,''_f''/''_f'',1),'), nl,
  write('trans(3,[]/ /,1),'), nl,
  write('trans(4,/ /[],2),'), nl,
  write('trans(4,$@(not_in([]))/ $@(not_in([])),4),'), nl,
  write('trans(5,[]/ /,4),'), nl,
  write('trans(6,''_t''/''_t'',5),'), nl,
  write('trans(6,/ /[],7),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,''_f''/''_f'',9),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(9,''_t''/''_t'',3),'), nl,
  write('trans(9,$@(not_in([]))/ $@(not_in([])),9)'), nl,
  write('], '), nl,
  write('[]). '), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generating the moving construction for variables within free(v1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_gen_free_moving([],_,_) :- !.

do_gen_free_moving([A|VarList],I,FullVarList) :-
  ( (I=1, !, S2=2, S3=3, S4=4, S5=5);
    (S2 is 8 + (I-2)*4,
     S3 is 8 + (I-2)*4 + 1,
     S4 is 8 + (I-2)*4 + 2,
     S5 is 8 + (I-2)*4 + 3 ) ),
  write('trans('),write(S2),write(',''_f''/''_f'',1),'), nl,
  write('trans('),write(S3),write(', '),write(A),write(' /[],'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([]))/ $@(not_in([])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',/ / /,'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in(['),
    gtv1(FullVarList,A,VarListGtA),
    gen_comma_sep_list(VarListGtA),
    write(']))/ $@(in(['),
    gen_comma_sep_list(VarListGtA),
    write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',[]/ '),write(A),write(' ,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(', '),write(A),write(' / '),write(A),write(' ,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    ltv1(FullVarList,A,VarListLtA),
    gen_comma_sep_list(VarListLtA),
    write(']))/ $@(in(['),
    gen_comma_sep_list(VarListLtA),
    write('])),'),write(S5),write('),'), nl,
  write('trans(6,!/!,'),write(S5),write('),'), 
  II is I+1,
  do_gen_free_moving(VarList,II,FullVarList).

%-------------------------------------------------------------------------------

gen_free_moving(FOut,VarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(VarList,Len),
  N is 8 + 4*(Len-1),  
  write(N),write(','), nl,  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,d/d,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  do_gen_free_moving(VarList,1,VarList),
  write('trans(7,$@(not_in([]))/ $@(not_in([])),6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generating the removal of data items/instrumentation labels within free(v1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_gen_free_remove([]) :- !.

do_gen_free_remove([Item|ItemList]) :-
  write('trans(3, '),write(Item),write(' /[],2),'), nl,
  do_gen_free_remove(ItemList).

%-------------------------------------------------------------------------------

gen_free_remove(FOut,ItemList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('6,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,d/d,5),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,''_f''/''_f'',1),'), nl,
  do_gen_free_remove(ItemList),
  write('trans(3,$@(not_in([]))/ $@(not_in([])),3),'), nl,
  write('trans(4,!/!,3),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),4)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generation of encoding of commands manipulating linear lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: if (v1->next == 0) goto c2; else goto c3
%-------------------------------------------------------------------------------

gen_if_V1_next_eq_null(FOut,C1,C2,C3,V1) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('20,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('[ '), nl,
  write('1,'), nl,
  write('2'), nl,
  write('], '), nl,
  write('['), nl,
  write('trans(0,n/e,10),'), nl,
  write('trans(0,n/n,11),'), nl,
  write('trans(1,/ / /,3),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(1,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),4),'), nl,
  write('trans(2,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(3,!/!,2),'), nl,
  write('trans(4,/ / /,3),'), nl,
  write('trans(4,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),4),'), nl,
  write('trans(5,# / #,6),'), nl,
  write('trans(5,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(5,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(6,'),write(V1),write('/'),write(V1),write(',2),'), nl,
  write('trans(6,$@(not_in([''|'']))/ $@(not_in([''|''])),6),'), nl,
  write('trans(7,'),write(V1),write('/'),write(V1),write(',4),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,# / #,6),'), nl,
  write('trans(8,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(9,'),write(V1),write('/'),write(V1),write(',2),'), nl,
  write('trans(9,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(10,'),write(C1),write('/'),write(C1),write(',5),'), nl,
  write('trans(11,'),write(C1),write('/'),write(C2),write(',15),'), nl,
  write('trans(11,'),write(C1),write('/'),write(C3),write(',16),'), nl,
  write('trans(12,# / #,2),'), nl,
  write('trans(13,/ / /,12),'), nl,
  write('trans(13,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),13),'), nl,
  write('trans(14,'),write(V1),write('/'),write(V1),write(',13),'), nl,
  write('trans(14,$@(not_in([]))/ $@(not_in([])),14),'), nl,
  write('trans(15,''|''/''|'',14),'), nl,
  write('trans(15,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),15),'), nl,
  write('trans(16,''|''/''|'',19),'), nl,
  write('trans(16,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),16),'), nl,
  write('trans(17,$@(not_in([!,#]))/ $@(not_in([!,#])),2),'), nl,
  write('trans(18,/ / /,17),'), nl,
  write('trans(18,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),18),'), nl,
  write('trans(19,'),write(V1),write('/'),write(V1),write(',18),'), nl,
  write('trans(19,$@(not_in([]))/ $@(not_in([])),19)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
%-------------------------------------------------------------------------------

gen_if_V1_next_V2(FOut,C1,C2,C3,V1,V2) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('30,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,8),'), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,3),'), nl,
  write('trans(2,'),write(V1),write('/'),write(V1),write(',4),'), nl,
  write('trans(2,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),1),'), nl,
  write('trans(2,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),6),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),7),'), nl,
  write('trans(3,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,/ / /,10),'), nl,
  write('trans(4,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),4),'), nl,
  write('trans(5,'),write(V1),write('/'),write(V1),write(',4),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(6,# / #,3),'), nl,
  write('trans(6,$@(not_in([#]))/ $@(not_in([#])),6),'), nl,
  write('trans(7,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),1),'), nl,
  write('trans(7,$@(not_in([/]))/ $@(not_in([/])),7),'), nl,
  write('trans(8,'),write(C1),write('/'),write(C1),write(',2),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C2),write(',14),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C3),write(',18),'), nl,
  write('trans(10,!/!,1),'), nl,
  write('trans(11,'),write(V2),write('/'),write(V2),write(',1),'), nl,
  write('trans(11,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),11),'), nl,
  write('trans(12,/ / /,11),'), nl,
  write('trans(12,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),12),'), nl,
  write('trans(13,'),write(V1),write('/'),write(V1),write(',12),'), nl,
  write('trans(13,$@(not_in([]))/ $@(not_in([])),13),'), nl,
  write('trans(14,# / #,15),'), nl,
  write('trans(14,''|''/''|'',13),'), nl,
  write('trans(14,$@(not_in([]))/ $@(not_in([])),16),'), nl,
  write('trans(14,$@(not_in([#]))/ $@(not_in([#])),17),'), nl,
  write('trans(15,'),write(V2),write('/'),write(V2),write(',29),'), nl,
  write('trans(15,$@(not_in([''|'']))/ $@(not_in([''|''])),15),'), nl,
  write('trans(16,''|''/''|'',13),'), nl,
  write('trans(16,$@(not_in([]))/ $@(not_in([])),16),'), nl,
  write('trans(17,# / #,15),'), nl,
  write('trans(17,$@(not_in([#]))/ $@(not_in([#])),17),'), nl,
  write('trans(18,# / #,22),'), nl,
  write('trans(18,/ / /,23),'), nl,
  write('trans(18,$@(not_in([#,'),write(V2),write(']))/ $@(not_in([#,'),write(V2),write('])),24),'), nl,
  write('trans(18,$@(not_in(['),write(V2),write(']))/ $@(not_in(['),write(V2),write('])),25),'), nl,
  write('trans(19,# / #,1),'), nl,
  write('trans(20,/ / /,19),'), nl,
  write('trans(20,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),20),'), nl,
  write('trans(21,'),write(V1),write('/'),write(V1),write(',20),'), nl,
  write('trans(21,$@(not_in([]))/ $@(not_in([])),21),'), nl,
  write('trans(22,''|''/''|'',21),'), nl,
  write('trans(22,$@(not_in(['),write(V2),write(',''|'']))/ $@(not_in(['),write(V2),write(',''|''])),22),'), nl,
  write('trans(23,''|''/''|'',28),'), nl,
  write('trans(23,$@(not_in([]))/ $@(not_in([])),23),'), nl,
  write('trans(24,# / #,22),'), nl,
  write('trans(24,$@(not_in([#,'),write(V2),write(']))/ $@(not_in([#,'),write(V2),write('])),24),'), nl,
  write('trans(25,/ / /,23),'), nl,
  write('trans(25,$@(not_in(['),write(V2),write(']))/ $@(not_in(['),write(V2),write('])),25),'), nl,
  write('trans(26,$@(in([/,''|'']))/ $@(in([/,''|''])),1),'), nl,
  write('trans(26,$@(not_in([/,#,!,'),write(V2),write(',''|'']))/ $@(not_in([/,#,!,'),write(V2),write(',''|''])),26),'), nl,
  write('trans(27,/ / /,26),'), nl,
  write('trans(27,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),27),'), nl,
  write('trans(28,'),write(V1),write('/'),write(V1),write(',27),'), nl,
  write('trans(28,$@(not_in([]))/ $@(not_in([])),28),'), nl,
  write('trans(29,''|''/''|'',21),'), nl,
  write('trans(29,$@(not_in([]))/ $@(not_in([])),29)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1 == 0) goto c2; else goto c3
%-------------------------------------------------------------------------------

gen_if_V1_eq_null(FOut,C1,C2,C3,V1) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('8, '), nl,
  write('[ '), nl,
  write('0'), nl,
  write('], '), nl,
  write('[ '), nl,
  write('1'), nl,
  write('], '), nl,
  write('[ '), nl,
  write('trans(0,n/e,3),'), nl,
  write('trans(0,n/n,4),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),2),'), nl,
  write('trans(3,'),write(C1),write('/'),write(C1),write(',2),'), nl,
  write('trans(4,'),write(C1),write('/'),write(C2),write(',6),'), nl,
  write('trans(4,'),write(C1),write('/'),write(C3),write(',5),'), nl,
  write('trans(5,''|''/''|'',1),'), nl,
  write('trans(5,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),5),'), nl,
  write('trans(6,# / #,7),'), nl,
  write('trans(6,$@(not_in([#]))/ $@(not_in([#])),6),'), nl,
  write('trans(7,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(7,$@(not_in([''|'']))/ $@(not_in([''|''])),7)'), nl,
  write('], '), nl,
  write('[]). '), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1 == v2) goto c2; else goto c3
%-------------------------------------------------------------------------------

gen_if_V1_eq_V2(FOut,C1,C2,C3,V1,V2) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('12,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,3),'), nl,
  write('trans(0,n/n,4),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),1),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),2),'), nl,
  write('trans(3,'),write(C1),write('/'),write(C1),write(',2),'), nl,
  write('trans(4,'),write(C1),write('/'),write(C2),write(',7),'), nl,
  write('trans(4,'),write(C1),write('/'),write(C3),write(',8),'), nl,
  write('trans(5,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),1),'), nl,
  write('trans(5,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(6,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),5),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,/ / /,6),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,/ / /,11),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),1),'), nl,
  write('trans(9,$@(not_in([]))/ $@(not_in([])),9),'), nl,
  write('trans(10,$@(in([/,''|'']))/ $@(in([/,''|''])),9),'), nl,
  write('trans(10,$@(not_in([]))/ $@(not_in([])),10),'), nl,
  write('trans(11,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),10),'), nl,
  write('trans(11,$@(not_in([]))/ $@(not_in([])),11)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next := 0; goto c2;
%-------------------------------------------------------------------------------

gen_V1_next_assign_null(FOut,C1,C2,V1,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('14,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,6),'), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,3),'), nl,
  write('trans(2,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write('/'),write(C1),write(',2),'), nl,
  write('trans(7,'),write(C1),write('/'),write(C2),write(',13),'), nl,
  write('trans(8,!/ #,1),'), nl,
  write('trans(8,# / #,1),'), nl,
  write('trans(8,[]/ #,9),'), nl,
  write('trans(9,[]/''|'',10),'), nl,
  write('trans(10,$@(not_in([!,#]))/ $@(not_in([!,#])),1),'), nl,
  write('trans(11,/ / /,8),'), nl,
  write('trans(11,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),11),'), nl,
  write('trans(12,'),write(V1),write('/'),write(V1),write(',11),'), nl,
  write('trans(12,$@(not_in([]))/ $@(not_in([])),12),'), nl,
  write('trans(13,''|''/''|'',12),'), nl,
  write('trans(13,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),13)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next := v2; goto c2;
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2(FOut,C1,C2,V1,V2) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('54,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,6),'), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(0,n/s,8),'), nl,
  write('trans(0,n/u,9),'), nl,
  write('trans(0,s/n,10),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,3),'), nl,
  write('trans(2,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,$@(in(['),write(V1),write(','),write(V2),write(']))/ $@(in(['),write(V1),write(','),write(V2),write('])),1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write('/'),write(C1),write(',2),'), nl,
  write('trans(7,'),write(C1),write('/'),write(C2),write(',23),'), nl,
  write('trans(8,'),write(C1),write('/'),write(C1),write(',50),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C1),write(',18),'), nl,
  write('trans(10,'),write(C1),write('/'),write(C2),write(',32),'), nl,
  write('trans(11,'),write(V2),write('/'),write(V2),write(',1),'), nl,
  write('trans(11,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),11),'), nl,
  write('trans(12,/ / /,11),'), nl,
  write('trans(12,'),write(V1),write('/'),write(V1),write(',13),'), nl,
  write('trans(12,'),write(V2),write('/'),write(V2),write(',3),'), nl,
  write('trans(12,$@(in([/,''|'']))/ $@(in([/,''|''])),14),'), nl,
  write('trans(12,$@(not_in([/]))/ $@(not_in([/])),15),'), nl,
  write('trans(12,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),16),'), nl,
  write('trans(12,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),17),'), nl,
  write('trans(13,'),write(V2),write('/'),write(V2),write(',1),'), nl,
  write('trans(13,$@(not_in([/]))/ $@(not_in([/])),13),'), nl,
  write('trans(14,/ / /,11),'), nl,
  write('trans(14,$@(in([/,''|'']))/ $@(in([/,''|''])),14),'), nl,
  write('trans(14,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),16),'), nl,
  write('trans(14,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),17),'), nl,
  write('trans(15,'),write(V1),write('/'),write(V1),write(',13),'), nl,
  write('trans(15,'),write(V2),write('/'),write(V2),write(',3),'), nl,
  write('trans(15,$@(not_in([/]))/ $@(not_in([/])),15),'), nl,
  write('trans(16,/ / /,11),'), nl,
  write('trans(16,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),16),'), nl,
  write('trans(17,$@(in([/,''|'']))/ $@(in([/,''|''])),14),'), nl,
  write('trans(17,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),17),'), nl,
  write('trans(18,''|''/''|'',12),'), nl,
  write('trans(18,$@(not_in([]))/ $@(not_in([])),19),'), nl,
  write('trans(18,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),20),'), nl,
  write('trans(19,''|''/''|'',15),'), nl,
  write('trans(19,$@(not_in([]))/ $@(not_in([])),19),'), nl,
  write('trans(20,''|''/''|'',14),'), nl,
  write('trans(20,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),20),'), nl,
  write('trans(21,/ / /,11),'), nl,
  write('trans(21,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),21),'), nl,
  write('trans(22,'),write(V1),write('/'),write(V1),write(',21),'), nl,
  write('trans(22,$@(not_in([]))/ $@(not_in([])),22),'), nl,
  write('trans(23,# / #,24),'), nl,
  write('trans(23,''|''/''|'',22),'), nl,
  write('trans(23,$@(not_in([]))/ $@(not_in([])),25),'), nl,
  write('trans(23,$@(not_in([#]))/ $@(not_in([#])),26),'), nl,
  write('trans(24,'),write(V2),write('/'),write(V2),write(',41),'), nl,
  write('trans(24,$@(not_in([''|'']))/ $@(not_in([''|''])),24),'), nl,
  write('trans(25,''|''/''|'',22),'), nl,
  write('trans(25,$@(not_in([]))/ $@(not_in([])),25),'), nl,
  write('trans(26,# / #,24),'), nl,
  write('trans(26,$@(not_in([#]))/ $@(not_in([#])),26),'), nl,
  write('trans(27,$@(not_in([!,#]))/ $@(not_in([!,#])),1),'), nl,
  write('trans(27,in([!,#])/[],28),'), nl,
  write('trans(28,''|''/[],1),'), nl,
  write('trans(29,''_t''/[],1),'), nl,
  write('trans(29,$@(not_in([]))/ $@(not_in([])),29),'), nl,
  write('trans(30,''_f''/[],27),'), nl,
  write('trans(31,''|''/''|'',30),'), nl,
  write('trans(31,$@(not_in([]))/ $@(not_in([])),31),'), nl,
  write('trans(32,''_t''/[],31),'), nl,
  write('trans(32,''|''/''|'',33),'), nl,
  write('trans(32,$@(not_in([]))/ $@(not_in([])),32),'), nl,
  write('trans(33,''_f''/[],34),'), nl,
  write('trans(34,$@(not_in([!,#]))/ $@(not_in([!,#])),29),'), nl,
  write('trans(34,in([!,#])/[],35),'), nl,
  write('trans(35,''|''/[],29),'), nl,
  write('trans(36,!/ #,1),'), nl,
  write('trans(36,# / #,1),'), nl,
  write('trans(36,[]/ #,37),'), nl,
  write('trans(37,[]/''|'',38),'), nl,
  write('trans(38,$@(not_in([!,#]))/ $@(not_in([!,#])),1),'), nl,
  write('trans(39,/ / /,36),'), nl,
  write('trans(39,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),39),'), nl,
  write('trans(40,'),write(V1),write('/'),write(V1),write(',39),'), nl,
  write('trans(40,$@(not_in([]))/ $@(not_in([])),40),'), nl,
  write('trans(41,''|''/''|'',40),'), nl,
  write('trans(41,$@(not_in([]))/ $@(not_in([])),41),'), nl,
  write('trans(42,/ /''_f'',1),'), nl,
  write('trans(42,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),42),'), nl,
  write('trans(43,'),write(V1),write('/'),write(V1),write(',42),'), nl,
  write('trans(43,$@(not_in([]))/ $@(not_in([])),43),'), nl,
  write('trans(44,''|''/''|'',43),'), nl,
  write('trans(44,$@(not_in([]))/ $@(not_in([])),44),'), nl,
  write('trans(45,'),write(V2),write('/'),write(V2),write(',44),'), nl,
  write('trans(45,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),45),'), nl,
  write('trans(46,[]/ /,45),'), nl,
  write('trans(47,[]/''_t'',46),'), nl,
  write('trans(47,'),write(V1),write('/'),write(V1),write(',48),'), nl,
  write('trans(47,$@(not_in(['),write(V1),write(','),write(V2),write(']))/ $@(not_in(['),write(V1),write(','),write(V2),write('])),49),'), nl,
  write('trans(48,/ /''_f'',53),'), nl,
  write('trans(48,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),48),'), nl,
  write('trans(49,'),write(V1),write('/'),write(V1),write(',48),'), nl,
  write('trans(49,$@(not_in(['),write(V1),write(','),write(V2),write(']))/ $@(not_in(['),write(V1),write(','),write(V2),write('])),49),'), nl,
  write('trans(50,''|''/''|'',47),'), nl,
  write('trans(50,$@(not_in(['),write(V1),write(','),write(V2),write(']))/ $@(not_in(['),write(V1),write(','),write(V2),write('])),50),'), nl,
  write('trans(51,[]/ /,11),'), nl,
  write('trans(52,[]/''_t'',51),'), nl,
  write('trans(53,''|''/''|'',52),'), nl,
  write('trans(53,$@(not_in([]))/ $@(not_in([])),53)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------

gen_V1_next_assign_V2(FOut1,FOut2,C1,C2,V1,V2,ItemVarList) :-
  gen_V1_next_assign_V2(FOut1,C1,C2,V1,V2),
  gen_V1_next_assign_V2_shifting(FOut2,ItemVarList).
  
%-------------------------------------------------------------------------------
% c1: v1 := 0; goto c2;
%-------------------------------------------------------------------------------

gen_V1_assign_null(FOut,C1,C2,V1,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('6,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,5),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  write('trans(2,$@(in(['),
    gtv1(ItemVarList,V1,ItemVarListGtV1),
    gen_comma_sep_list(ItemVarListGtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write('])),2),'), nl,
  write('trans(2,''|''/''|'',1),'), nl,
  write('trans(3,[]/'),write(V1),write(',2),'), nl,
  write('trans(3,$@(in(['),
    ltv1(ItemVarList,V1,ItemVarListLtV1),
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),3),'), nl,
  write('trans(3,'),write(V1),write(''/''),write(V1),write(',2),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,'),write(V1),write('/[],4),'), nl,
  write('trans(4,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),4),'), nl,
  write('trans(5,'),write(C1),write(''/''),write(C2),write(',4)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 := v2->next; goto c2;
%-------------------------------------------------------------------------------

%%% The V1=V2 version has been created additionally by a dirty hack of the basic version...

gen_V1_assign_V2_next(FOut,C1,C2,V1,V2,ItemVarList) :-
  V1=V2,
  !,
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('25, '), nl,
  write('[ '), nl,
  write('0'), nl,
  write('], '), nl,
  write('[ '), nl,
  write('1,'), nl,
  write('2,'), nl,
  write('3'), nl,
  write('], '), nl,
  write('[ '), nl,
  write('trans(0,n/e,11),'), nl,
  write('trans(0,n/n,12),'), nl,
  write('trans(1,/ / /,4),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(1,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(2,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(3,'),write(V1),write('/[],3),'), nl,
  write('trans(3,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),3),'), nl,
  write('trans(4,!/!,2),'), nl,
  write('trans(5,/ / /,4),'), nl,
  write('trans(5,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(6,# / #,7),'), nl,
  write('trans(6,'),write(V2),write('/[],1),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(6,$@(not_in([#]))/ $@(not_in([#])),9),'), nl,
  write('trans(6,$@(not_in([/]))/ $@(not_in([/])),10),'), nl,
  write('trans(7,'),write(V2),write('/[],2),'), nl,
  write('trans(7,$@(not_in([''|'']))/ $@(not_in([''|''])),7),'), nl,
  write('trans(8,'),write(V2),write('/[],5),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,# / #,7),'), nl,
  write('trans(9,$@(not_in([#]))/ $@(not_in([#])),9),'), nl,
  write('trans(10,'),write(V2),write('/[],2),'), nl,
  write('trans(10,$@(not_in([/]))/ $@(not_in([/])),10),'), nl,
  write('trans(11,'),write(C1),write('/'),write(C1),write(',6),'), nl,
  write('trans(12,'),write(C1),write('/'),write(C2),write(',17),'), nl,
  write('trans(13,/ / /,3),'), nl,
  write('trans(13,$@(in(['),
    gtv1(ItemVarList,V1,ItemVarListGtV1),
    gen_comma_sep_list(ItemVarListGtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write('])),13),'), nl,
  write('trans(14,[]/'),write(V1),write(',13),'), nl,
  write('trans(14,$@(in(['),
    ltv1(ItemVarList,V1,ItemVarListLtV1),
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),14),'), nl,
  write('trans(14,'),write(V1),write('/'),write(V1),write(',13),'), nl,
  write('trans(15,/ / /,14),'), nl,
  write('trans(15,'),write(V1),write('/[],15),'), nl,
  write('trans(15,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),15),'), nl,
  write('trans(16,'),write(V1),write('/[],16),'), nl,
  write('trans(16,'),write(V2),write('/[],15),'), nl,
  write('trans(16,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),16),'), nl,
  write('trans(17,# / #,18),'), nl,
  write('trans(17,'),write(V1),write('/[],17),'), nl,
  write('trans(17,''|''/''|'',16),'), nl,
  write('trans(17,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),19),'), nl,
  write('trans(17,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),20),'), nl,
  write('trans(18,[]/'),write(V1),write(',24),'), nl,
  write('trans(18,$@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),18),'), nl,
  write('trans(18,'),write(V1),write('/'),write(V1),write(',24),'), nl,
  write('trans(19,# / #,18),'), nl,
  write('trans(19,'),write(V1),write('/[],19),'), nl,
  write('trans(19,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),19),'), nl,
  write('trans(20,'),write(V1),write('/[],20),'), nl,
  write('trans(20,''|''/''|'',16),'), nl,
  write('trans(20,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),20),'), nl,
  write('trans(21,# / #,3),'), nl,
  write('trans(22,/ / /,21),'), nl,
  write('trans(22,'),write(V1),write('/[],22),'), nl,
  write('trans(22,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),22),'), nl,
  write('trans(23,'),write(V1),write('/[],23),'), nl,
  write('trans(23,'),write(V2),write('/[],22),'), nl,
  write('trans(23,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),23),'), nl,
  write('trans(24,$@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write('])),24),'), nl,
  write('trans(24,''|''/''|'',23)'), nl,
  write('], '), nl,
  write('[]). '), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------

gen_V1_assign_V2_next(FOut,C1,C2,V1,V2,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('25, '), nl,
  write('[ '), nl,
  write('0'), nl,
  write('], '), nl,
  write('[ '), nl,
  write('1,'), nl,
  write('2,'), nl,
  write('3'), nl,
  write('], '), nl,
  write('[ '), nl,
  write('trans(0,n/e,11),'), nl,
  write('trans(0,n/n,12),'), nl,
  write('trans(1,/ / /,4),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(1,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(2,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(3,'),write(V1),write('/[],3),'), nl,
  write('trans(3,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),3),'), nl,
  write('trans(4,!/!,2),'), nl,
  write('trans(5,/ / /,4),'), nl,
  write('trans(5,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(6,# / #,7),'), nl,
  write('trans(6,'),write(V2),write('/'),write(V2),write(',1),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(6,$@(not_in([#]))/ $@(not_in([#])),9),'), nl,
  write('trans(6,$@(not_in([/]))/ $@(not_in([/])),10),'), nl,
  write('trans(7,'),write(V2),write('/'),write(V2),write(',2),'), nl,
  write('trans(7,$@(not_in([''|'']))/ $@(not_in([''|''])),7),'), nl,
  write('trans(8,'),write(V2),write('/'),write(V2),write(',5),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,# / #,7),'), nl,
  write('trans(9,$@(not_in([#]))/ $@(not_in([#])),9),'), nl,
  write('trans(10,'),write(V2),write('/'),write(V2),write(',2),'), nl,
  write('trans(10,$@(not_in([/]))/ $@(not_in([/])),10),'), nl,
  write('trans(11,'),write(C1),write('/'),write(C1),write(',6),'), nl,
  write('trans(12,'),write(C1),write('/'),write(C2),write(',17),'), nl,
  write('trans(13,/ / /,3),'), nl,
  write('trans(13,$@(in(['),
    gtv1(ItemVarList,V1,ItemVarListGtV1),
    gen_comma_sep_list(ItemVarListGtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write('])),13),'), nl,
  write('trans(14,[]/'),write(V1),write(',13),'), nl,
  write('trans(14,$@(in(['),
    ltv1(ItemVarList,V1,ItemVarListLtV1),
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),14),'), nl,
  write('trans(14,'),write(V1),write('/'),write(V1),write(',13),'), nl,
  write('trans(15,/ / /,14),'), nl,
  write('trans(15,'),write(V1),write('/[],15),'), nl,
  write('trans(15,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),15),'), nl,
  write('trans(16,'),write(V1),write('/[],16),'), nl,
  write('trans(16,'),write(V2),write('/'),write(V2),write(',15),'), nl,
  write('trans(16,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),16),'), nl,
  write('trans(17,# / #,18),'), nl,
  write('trans(17,'),write(V1),write('/[],17),'), nl,
  write('trans(17,''|''/''|'',16),'), nl,
  write('trans(17,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),19),'), nl,
  write('trans(17,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),20),'), nl,
  write('trans(18,[]/'),write(V1),write(',24),'), nl,
  write('trans(18,$@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),18),'), nl,
  write('trans(18,'),write(V1),write('/'),write(V1),write(',24),'), nl,
  write('trans(19,# / #,18),'), nl,
  write('trans(19,'),write(V1),write('/[],19),'), nl,
  write('trans(19,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),19),'), nl,
  write('trans(20,'),write(V1),write('/[],20),'), nl,
  write('trans(20,''|''/''|'',16),'), nl,
  write('trans(20,$@(not_in(['),write(V1),write(',''|'']))/ $@(not_in(['),write(V1),write(',''|''])),20),'), nl,
  write('trans(21,# / #,3),'), nl,
  write('trans(22,/ / /,21),'), nl,
  write('trans(22,'),write(V1),write('/[],22),'), nl,
  write('trans(22,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),22),'), nl,
  write('trans(23,'),write(V1),write('/[],23),'), nl,
  write('trans(23,'),write(V2),write('/'),write(V2),write(',22),'), nl,
  write('trans(23,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),23),'), nl,
  write('trans(24,$@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write('])),24),'), nl,
  write('trans(24,''|''/''|'',23)'), nl,
  write('], '), nl,
  write('[]). '), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 := v2; goto c2;
%-------------------------------------------------------------------------------

gen_V1_assign_V2(FOut,C1,C2,V1,V2,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  ItemVarListExt=['#'|ItemVarList],
  ( (is_v1v2(ItemVarListExt,V1,V2), !, gen_V1_assign_V2_V1V2(ItemVarListExt,C1,C2,V1,V2));
    gen_V1_assign_V2_V2V1(ItemVarListExt,C1,C2,V1,V2) ),
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm),
  !.
  
%................................................................................

gen_V1_assign_V2_V1V2(ItemVarList,C1,C2,V1,V2) :-
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('9,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1,'), nl,
  write('2'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,4),'), nl,
  write('trans(0,n/n,5),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,'),write(V1),write('/[],2),'), nl,
  write('trans(2,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),2),'), nl,
  write('trans(3,'),write(V2),write('/'),write(V2),write(',1),'), nl,
  write('trans(3,$@(not_in([/]))/ $@(not_in([/])),3),'), nl,
  write('trans(4,'),write(C1),write('/'),write(C1),write(',3),'), nl,
  write('trans(5,'),write(C1),write('/'),write(C2),write(',8),'), nl,
  write('trans(6,$@(in(['),
    v1v2(ItemVarList,V1,V2,ItemVarListV1V2),
    gen_comma_sep_list(ItemVarListV1V2),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListV1V2),
    write('])),6),'), nl,
  write('trans(6,'),write(V2),write('/'),write(V2),write(',2),'), nl,
  write('trans(7,[]/'),write(V1),write(',6),'), nl,
  write('trans(7,$@(in(['),
    ltv1(ItemVarList,V1,ItemVarListLtV1),
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),7),'), nl,
  write('trans(7,'),write(V1),write('/'),write(V1),write(',6),'), nl,
  write('trans(8,'),write(V1),write('/[],8),'), nl,
  write('trans(8,$@(in([/,''|'']))/ $@(in([/,''|''])),7),'), nl,
  write('trans(8,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),8)'), nl,
  write('],'), nl,
  write('[]).'), nl.

%................................................................................
  
gen_V1_assign_V2_V2V1(ItemVarList,C1,C2,V1,V2) :-
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('10,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1,'), nl,
  write('2'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,4),'), nl,
  write('trans(0,n/n,5),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,'),write(V1),write('/[],2),'), nl,
  write('trans(2,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),2),'), nl,
  write('trans(3,'),write(V2),write('/'),write(V2),write(',1),'), nl,
  write('trans(3,$@(not_in([/]))/ $@(not_in([/])),3),'), nl,
  write('trans(4,'),write(C1),write('/'),write(C1),write(',3),'), nl,
  write('trans(5,'),write(C1),write('/'),write(C2),write(',9),'), nl,
  write('trans(6,$@(in(['),
    gtv1(ItemVarList,V1,ItemVarListGtV1),
    gen_comma_sep_list(ItemVarListGtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write('])),6),'), nl,
  write('trans(6,$@(in([/,''|'']))/ $@(in([/,''|''])),2),'), nl,
  write('trans(7,[]/'),write(V1),write(',6),'), nl,
  write('trans(7,'),write(V1),write('/'),write(V1),write(',6),'), nl,
  write('trans(7,$@(in(['),
    v1v2(ItemVarList,V2,V1,ItemVarListV2V1),
    gen_comma_sep_list(ItemVarListV2V1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListV2V1),
    write('])),7),'), nl,
  write('trans(8,'),write(V1),write('/[],8),'), nl,
  write('trans(8,'),write(V2),write('/'),write(V2),write(',7),'), nl,
  write('trans(8,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),8),'), nl,
  write('trans(9,/ / /,8),'), nl,
  write('trans(9,'),write(V1),write('/[],9),'), nl,
  write('trans(9,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),9)'), nl,
  write('],'), nl,
  write('[]).'), nl.

%-------------------------------------------------------------------------------
% c1: malloc(v1); goto c2; (if there is not enough memory, v1:=null)
% The next pointer of v1 is undefined, no data are associated with v1.
%-------------------------------------------------------------------------------

gen_malloc_list_elm_V1(FOut,C1,C2,V1,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('12,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1,'), nl,
  write('2'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  write('trans(3,$@(in(['),
    gtv1(ItemVarList,V1,ItemVarListGtV1),
    gen_comma_sep_list(ItemVarListGtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListGtV1),
    write('])),3),'), nl,
  write('trans(3,''|''/''|'',1),'), nl,
  write('trans(4,[]/'),write(V1),write(',3),'), nl,
  write('trans(4,$@(in(['),
    ltv1(ItemVarList,V1,ItemVarListLtV1),
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),4),'), nl,
  write('trans(4,'),write(V1),write('/'),write(V1),write(',3),'), nl,
  write('trans(5,# / #,4),'), nl,
  write('trans(5,[]/'),write(V1),write(',6),'), nl,
  write('trans(5,'),write(V1),write('/[],5),'), nl,
  write('trans(5,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),7),'), nl,
  write('trans(5,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),8),'), nl,
  write('trans(6,[]/ /,11),'), nl,
  write('trans(7,# / #,4),'), nl,
  write('trans(7,'),write(V1),write('/[],7),'), nl,
  write('trans(7,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),7),'), nl,
  write('trans(8,[]/'),write(V1),write(',6),'), nl,
  write('trans(8,'),write(V1),write('/[],8),'), nl,
  write('trans(8,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),8),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C2),write(',5),'), nl,
  write('trans(10,[]/''|'',2),'), nl,
  write('trans(11,[]/!,10)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  write(''), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: free(v1); goto c2;
%-------------------------------------------------------------------------------

gen_free_list_elm_V1(FOut,C1,C2,V1) :-
  open(':aux',write,SOut),
  tell(SOut),  
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('23,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,d/n,7),'), nl,
  write('trans(0,n/d,8),'), nl,
  write('trans(0,n/e,6),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,3),'), nl,
  write('trans(2,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V1),write('/'),write(V1),write(',1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write('/'),write(C1),write(',2),'), nl,
  write('trans(7,'),write(C1),write('/'),write(C2),write(',13),'), nl,
  write('trans(8,'),write(C1),write('/'),write(C1),write(',22),'), nl,
  write('trans(9,$@(not_in([!,#]))/ $@(not_in([!,#])),1),'), nl,
  write('trans(10,/ /''|'',9),'), nl,
  write('trans(11,''_f''/!,10),'), nl,
  write('trans(11,''_f''/[],12),'), nl,
  write('trans(12,/ /[],15),'), nl,
  write('trans(13,/ / /,11),'), nl,
  write('trans(13,''|''/''|'',14),'), nl,
  write('trans(13,$@(not_in([]))/ $@(not_in([])),13),'), nl,
  write('trans(14,''_f''/[],18),'), nl,
  write('trans(15,in([!,#])/!,1),'), nl,
  write('trans(16,$@(not_in([!,#]))/ $@(not_in([!,#])),1),'), nl,
  write('trans(16,in([!,#])/[],17),'), nl,
  write('trans(17,''|''/[],1),'), nl,
  write('trans(18,/ /[],16),'), nl,
  write('trans(19,/ / /,1),'), nl,
  write('trans(20,[]/''_f'',19),'), nl,
  write('trans(20,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),20),'), nl,
  write('trans(21,'),write(V1),write('/'),write(V1),write(',20),'), nl,
  write('trans(21,$@(not_in([]))/ $@(not_in([])),21),'), nl,
  write('trans(22,''|''/''|'',21),'), nl,
  write('trans(22,$@(not_in([]))/ $@(not_in([])),22)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%................................................................................

gen_free_list_elm_V1(FOut1,FOut2,FOut3,C1,C2,V1,VarList,ItemList) :-
  gen_free_list_elm_V1(FOut1,C1,C2,V1),
  gen_free_moving(FOut2,VarList),
  gen_free_remove(FOut3,ItemList).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorting items on the arcs of generated transducers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_one_trans(trans(A,$@(in(X))/ $@(in(Y)),B),trans(A,$@(in(XX))/ $@(in(YY)),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,$@(not_in(X))/ $@(not_in(Y)),B),trans(A,$@(not_in(XX))/ $@(not_in(YY)),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,$@(in(X))/ $@(not_in(Y)),B),trans(A,$@(in(XX))/ $@(not_in(YY)),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,$@(not_in(X))/ $@(in(Y)),B),trans(A,$@(not_in(XX))/ $@(in(YY)),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,in(X)/ in(Y),B),trans(A,in(XX)/ in(YY),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,not_in(X)/ not_in(Y),B),trans(A,not_in(XX)/ not_in(YY),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,in(X)/ not_in(Y),B),trans(A,in(XX)/ not_in(YY),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,not_in(X)/ in(Y),B),trans(A,not_in(XX)/ in(YY),B)) :-
  sort(X,XX),
  sort(Y,YY),
  !.

sort_one_trans(trans(A,in(X)/ Y,B),trans(A,in(XX)/ Y,B)) :-
  sort(X,XX),
  !.

sort_one_trans(trans(A,X/ in(Y),B),trans(A,X/ in(YY),B)) :-
  sort(Y,YY),
  !.

sort_one_trans(trans(A,not_in(X)/ Y,B),trans(A,not_in(XX)/ Y,B)) :-
  sort(X,XX),
  !.

sort_one_trans(trans(A,X/ not_in(YY),B),trans(A,X/ not_in(YY),B)) :-
  sort(Y,YY),
  !.

sort_one_trans(trans(A,X,B),trans(A,X,B)).

%------------------------------------------------------------------------------

sort_trans(fa(Pred,QN,IL,FL,TrL,[]),fa(Pred,QN,IL,FL,NewTrL,[])) :-
  do_sort_trans(TrL,NewTrL).

do_sort_trans([],[]) :- !.

do_sort_trans([Tr|TrL],[NewTr|NewTrL]) :-
  sort_one_trans(Tr,NewTr),
  do_sort_trans(TrL,NewTrL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


