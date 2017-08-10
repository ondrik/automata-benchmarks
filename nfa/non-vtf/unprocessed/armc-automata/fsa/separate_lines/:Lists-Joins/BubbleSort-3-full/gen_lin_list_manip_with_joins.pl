:- consult('/usr/local/src/fsa6/yap/fsa_library').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Working with ordering of data items, markers, and pointer variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%-------------------------------------------------------------------------------

do_gen_comma_sep_list_mf([]) :- !.

do_gen_comma_sep_list_mf([X|List]) :- write(','),write(X),write('_f '),do_gen_comma_sep_list_mf(List).

%...............................................................................

gen_comma_sep_list_mf([]) :- !.

gen_comma_sep_list_mf([X|List]) :- write(X),write('_f '),do_gen_comma_sep_list_mf(List).

%-------------------------------------------------------------------------------

do_gen_comma_sep_list_mt([]) :- !.

do_gen_comma_sep_list_mt([X|List]) :- write(','),write(X),write('_t '),do_gen_comma_sep_list_mt(List).


%...............................................................................

gen_comma_sep_list_mt([]) :- !.

gen_comma_sep_list_mt([X|List]) :- write(X),write('_t '),do_gen_comma_sep_list_mt(List).

%-------------------------------------------------------------------------------

gen_all_from_markers([Marker]) :- write(Marker),write('_f'),!.

gen_all_from_markers([Marker|Markers]) :- 
  write(Marker),write('_f,'),
  gen_all_from_markers(Markers).

%-------------------------------------------------------------------------------

gen_all_to_markers([Marker]) :- write(Marker),write('_t'),!.

gen_all_to_markers([Marker|Markers]) :- 
  write(Marker),write('_t,'),
  gen_all_to_markers(Markers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generation of encoding of commands manipulating linear lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: if (v1->next == v2) goto c2; else goto c3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The basic situations
%-------------------------------------------------------------------------------

gen_if_V1_next_V2_basic(FOut,C1,C2,C3,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('38,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1,'), nl,
  write('2'), nl,
  write('],'), nl,
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
  write('trans(5,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(5,'),write(V2),write(' / '),write(V2),write(',2),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(5,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(6,'),write(V1),write(' / '),write(V1),write(',2),'), nl,
  write('trans(6,$@(not_in([''|'']))/ $@(not_in([''|''])),6),'), nl,
  write('trans(7,'),write(V1),write(' / '),write(V1),write(',4),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,# / #,6),'), nl,
  write('trans(8,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(9,'),write(V1),write(' / '),write(V1),write(',2),'), nl,
  write('trans(9,'),write(V2),write(' / '),write(V2),write(',2),'), nl,
  write('trans(9,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(10,'),write(C1),write(' / '),write(C1),write(',5),'), nl,
  write('trans(11,'),write(C1),write(' / '),write(C2),write(',15),'), nl,
  write('trans(11,'),write(C1),write(' / '),write(C3),write(',19),'), nl,
  write('trans(12,'),write(V2),write(' / '),write(V2),write(',2),'), nl,
  write('trans(12,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),12),'), nl,
  write('trans(13,/ / /,12),'), nl,
  write('trans(13,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),13),'), nl,
  write('trans(14,'),write(V1),write(' / '),write(V1),write(',13),'), nl,
  write('trans(14,$@(not_in([]))/ $@(not_in([])),14),'), nl,
  write('trans(15,# / #,16),'), nl,
  write('trans(15,''|''/''|'',14),'), nl,
  write('trans(15,$@(not_in([]))/ $@(not_in([])),17),'), nl,
  write('trans(15,$@(not_in([#]))/ $@(not_in([#])),18),'), nl,
  write('trans(16,'),write(V2),write(' / '),write(V2),write(',34),'), nl,
  write('trans(16,$@(not_in([''|'']))/ $@(not_in([''|''])),16),'), nl,
  write('trans(17,''|''/''|'',14),'), nl,
  write('trans(17,$@(not_in([]))/ $@(not_in([])),17),'), nl,
  write('trans(18,# / #,16),'), nl,
  write('trans(18,$@(not_in([#]))/ $@(not_in([#])),18),'), nl,
  write('trans(19,# / #,23),'), nl,
  write('trans(19,/ / /,27),'), nl,
  write('trans(19,$@(not_in([#]))/ $@(not_in([#])),28),'), nl,
  write('trans(19,$@(not_in([#,'),write(V2),write(']))/ $@(not_in([#,'),write(V2),write('])),29),'), nl,
  write('trans(19,$@(not_in(['),write(V2),write(']))/ $@(not_in(['),write(V2),write('])),30),'), nl,
  write('trans(20,# / #,2),'), nl,
  write('trans(21,/ / /,20),'), nl,
  write('trans(21,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),21),'), nl,
  write('trans(22,'),write(V1),write(' / '),write(V1),write(',21),'), nl,
  write('trans(22,$@(not_in([]))/ $@(not_in([])),22),'), nl,
  write('trans(23,'),write(V2),write(' / '),write(V2),write(',24),'), nl,
  write('trans(23,''|''/''|'',22),'), nl,
  write('trans(23,$@(not_in(['),write(V2),write(',''|'']))/ $@(not_in(['),write(V2),write(',''|''])),25),'), nl,
  write('trans(23,$@(not_in([''|'']))/ $@(not_in([''|''])),26),'), nl,
  write('trans(24,''|''/''|'',37),'), nl,
  write('trans(24,$@(not_in([]))/ $@(not_in([])),24),'), nl,
  write('trans(25,''|''/''|'',22),'), nl,
  write('trans(25,$@(not_in(['),write(V2),write(',''|'']))/ $@(not_in(['),write(V2),write(',''|''])),25),'), nl,
  write('trans(26,'),write(V2),write(' / '),write(V2),write(',24),'), nl,
  write('trans(26,$@(not_in([''|'']))/ $@(not_in([''|''])),26),'), nl,
  write('trans(27,''|''/''|'',33),'), nl,
  write('trans(27,$@(not_in([]))/ $@(not_in([])),27),'), nl,
  write('trans(28,# / #,26),'), nl,
  write('trans(28,$@(not_in([#]))/ $@(not_in([#])),28),'), nl,
  write('trans(29,# / #,25),'), nl,
  write('trans(29,$@(not_in([#,'),write(V2),write(']))/ $@(not_in([#,'),write(V2),write('])),29),'), nl,
  write('trans(30,/ / /,27),'), nl,
  write('trans(30,$@(not_in(['),write(V2),write(']))/ $@(not_in(['),write(V2),write('])),30),'), nl,
  write('trans(31,$@(in([/,''|'']))/ $@(in([/,''|''])),2),'), nl,
  write('trans(31,$@(not_in([!,#,/,'), nl,
    gen_all_from_markers(Markers),
    write(','),write(V2),write(',''|'']))/ $@(not_in([!,#,/,'), nl,
    gen_all_from_markers(Markers),
    write(','),write(V2),write(',''|''])),31),'), nl,
  write('trans(32,/ / /,31),'), nl,
  write('trans(32,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),32),'), nl,
  write('trans(33,'),write(V1),write(' / '),write(V1),write(',32),'), nl,
  write('trans(33,$@(not_in([]))/ $@(not_in([])),33),'), nl,
  write('trans(34,''|''/''|'',22),'), nl,
  write('trans(34,$@(not_in([]))/ $@(not_in([])),34),'), nl,
  write('trans(35,$@(in(['), nl,
    gen_all_from_markers(Markers),
    write(']))/ $@(in(['), nl,
    gen_all_from_markers(Markers),
    write('])),2),'), nl,
  write('trans(36,/ / /,35),'), nl,
  write('trans(36,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),36),'), nl,
  write('trans(37,'),write(V1),write(' / '),write(V1),write(',36),'), nl,
  write('trans(37,$@(not_in([]))/ $@(not_in([])),37)'), nl,
  write('],'), nl,
  write('[]). '), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The basic situations for v1 being the same as v2.
%-------------------------------------------------------------------------------

gen_if_V1_next_V2_basic_for_V1_eq_V2(FOut,C1,C2,C3,V12,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('16,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1,'), nl,
  write('2'), nl,
  write('],'), nl,
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
  write('trans(5,'),write(V12),write(' / '),write(V12),write(',1),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(5,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(6,'),write(V12),write(' / '),write(V12),write(',2),'), nl,
  write('trans(6,$@(not_in([''|'']))/ $@(not_in([''|''])),6),'), nl,
  write('trans(7,'),write(V12),write(' / '),write(V12),write(',4),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,# / #,6),'), nl,
  write('trans(8,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(9,'),write(V12),write(' / '),write(V12),write(',2),'), nl,
  write('trans(9,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(10,'),write(C1),write(' / '),write(C1),write(',5),'), nl,
  write('trans(11,'),write(C1),write(' / '),write(C3),write(',15),'), nl,
  write('trans(12,$@(in([/,''|'']))/ $@(in([/,''|''])),2),'), nl,
  write('trans(12,$@(not_in([!,/,'), nl,
    gen_all_from_markers(Markers),
    write(',''|'']))/ $@(not_in([!,/,'), nl,
    gen_all_from_markers(Markers),
    write(',''|''])),12),'), nl,
  write('trans(13,/ / /,12),'), nl,
  write('trans(13,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),13),'), nl,
  write('trans(14,'),write(V12),write(' / '),write(V12),write(',13),'), nl,
  write('trans(14,$@(not_in([]))/ $@(not_in([])),14),'), nl,
  write('trans(15,''|''/''|'',14),'), nl,
  write('trans(15,$@(not_in([]))/ $@(not_in([])),15)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2=mt, and the v1 list item precedes the v2 item
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_true_a([],_) :- !.

gen_branches_if_V1_next_V2_true_a([Marker|Markers],I) :-
  ( (I=1, !, S3=3, S4=4);
    (S3 is 9 + (I-2)*2 + 1,
     S4 is 9 + (I-2)*2 + 2 ) ),
  write('trans('),write(S3),
    write(','),write(Marker),write('_t /'),write(Marker),write('_t ,2),'), nl,
  write('trans('),write(S3),write(',$@(not_in([]))/ $@(not_in([])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',''|''/''|'','),write(S3),write('),'), nl,
  write('trans(5,'),write(Marker),write('_f /'),write(Marker),write('_f ,'),
    write(S4),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_true_a(Markers,II).

%...............................................................................

gen_if_V1_next_V2_true_a(FOut,C1,C2,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + 2*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(2,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),2),'), nl,
  gen_branches_if_V1_next_V2_true_a(Markers,1),
  write('trans(6,/ / /,5),'), nl,
  write('trans(6,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),6),'), nl,
  write('trans(7,'),write(V1),write(' / '),write(V1),write(',6),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,''|''/''|'',7),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,'),write(C1),write(' / '),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2=mt, and the v2 list item precedes the v1 item
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_true_b(_,_,[],_) :- !.

gen_branches_if_V1_next_V2_true_b(V1,V2,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S4=4, S5=5, S6=6);
    (S2 is 9 + (I-2)*5 + 1,
     S3 is 9 + (I-2)*5 + 2,
     S4 is 9 + (I-2)*5 + 3,
     S5 is 9 + (I-2)*5 + 4,
     S6 is 9 + (I-2)*5 + 5) ),
  write('trans('),write(S2),
    write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' / '),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([]))/ $@(not_in([])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(','),write(V2),write(' / '),write(V2),write(','),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S6),write('),'), nl,
  write('trans(7,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),
    write(S6),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_true_b(V1,V2,Markers,II).

%...............................................................................

gen_if_V1_next_V2_true_b(FOut,C1,C2,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + 5*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_true_b(V1,V2,Markers,1),
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,''|''/''|'',7),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,'),write(C1),write(' / '),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2=mt, v1 and v2 point to the same list item,
%   but v1 precedes v2 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_true_c(_,_,[],_) :- !.

gen_branches_if_V1_next_V2_true_c(V1,V2,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S4=4, S5=5);
    (S2 is 8 + (I-2)*4 + 1,
     S3 is 8 + (I-2)*4 + 2,
     S4 is 8 + (I-2)*4 + 3,
     S5 is 8 + (I-2)*4 + 4) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V2),write(' / '),write(V2),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(','),write(V1),write(' / '),write(V1),write(','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S5),write('),'), nl,
  write('trans(6,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_true_c(V1,V2,Markers,II).

%...............................................................................

gen_if_V1_next_V2_true_c(FOut,C1,C2,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 9 + 4*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_true_c(V1,V2,Markers,1),
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,''|''/''|'',6),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,'),write(C1),write(' / '),write(C2),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2=mt, v1 and v2 point to the same list item,
%   but v2 precedes v1 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_true_d(_,_,[],_) :- !.

gen_branches_if_V1_next_V2_true_d(V1,V2,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S4=4, S5=5);
    (S2 is 8 + (I-2)*4 + 1,
     S3 is 8 + (I-2)*4 + 2,
     S4 is 8 + (I-2)*4 + 3,
     S5 is 8 + (I-2)*4 + 4) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' / '),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(','),write(V2),write(' / '),write(V2),write(','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S5),write('),'), nl,
  write('trans(6,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_true_d(V1,V2,Markers,II).

%...............................................................................

gen_if_V1_next_V2_true_d(FOut,C1,C2,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 9 + 4*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_true_d(V1,V2,Markers,1),
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,''|''/''|'',6),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,'),write(C1),write(' / '),write(C2),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2=mt, v1 and v2 point to the same list item,
%   and they are in fact the same variable.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_true_e(_,[],_) :- !.

gen_branches_if_V1_next_V2_true_e(V12,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S4=4);
    (S2 is 7 + (I-2)*3 + 1,
     S3 is 7 + (I-2)*3 + 2,
     S4 is 7 + (I-2)*3 + 3) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V12),write(' / '),write(V12),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans(5,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),write(S4),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_true_e(V12,Markers,II).

%...............................................................................

gen_if_V1_next_V2_true_e(FOut,C1,C2,V12,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 8 + 3*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_true_e(V12,Markers,1),
  write('trans(5,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(6,''|''/''|'',5),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,'),write(C1),write(' / '),write(C2),write(',6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2!=mt, and the v1 list item precedes the v2 item.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_false_a(_,_,[],_) :- !.

gen_branches_if_V1_next_V2_false_a(V1,V2,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3);
    (S2 is 8 + (I-2)*2 + 1,
     S3 is 8 + (I-2)*2 + 2) ),
  write('trans('),write(S2),write(','),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans('),write(S2),write(',$@(not_in([/,'),write(Marker),
    write('_t,''|'']))/ $@(not_in([/,'),write(Marker),write('_t,''|''])),'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([]))/ $@(not_in([])),'),write(S3),write('),'), nl,
  write('trans(4,'),write(Marker),write('_f /'),write(Marker),write('_f ,'),write(S3),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_false_a(V1,V2,Markers,II).

%...............................................................................

gen_if_V1_next_V2_false_a(FOut,C1,C3,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 9 + 2*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_false_a(V1,V2,Markers,1),
  write('trans(5,/ / /,4),'), nl,
  write('trans(5,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(6,'),write(V1),write(' / '),write(V1),write(',5),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,''|''/''|'',6),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,'),write(C1),write(' / '),write(C3),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2!=mt, and the v2 list item precedes the v1 item.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_false_b(_,_,[],_) :- !.

gen_branches_if_V1_next_V2_false_b(V1,V2,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S4=4, S5=5, S8=8);
    (S2 is 10 + (I-2)*5 + 1,
     S3 is 10 + (I-2)*5 + 2,
     S4 is 10 + (I-2)*5 + 3,
     S5 is 10 + (I-2)*5 + 4,
     S8 is 10 + (I-2)*5 + 5) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' / '),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl, 
  write('trans('),write(S5),write(',/ / /,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([]))/ $@(not_in([])),'),write(S5),write('),'), nl,
  write('trans(6,'),write(V2),write(' / '),write(V2),write(','),write(S5),write('),'), nl,
  write('trans(6,$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S8),write('),'), nl,
  write('trans('),write(S8),write(','),write(V2),write(' / '),write(V2),write(','),write(S5),write('),'), nl,
  write('trans('),write(S8),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S8),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_false_b(V1,V2,Markers,II).

%...............................................................................

gen_if_V1_next_V2_false_b(FOut,C1,C3,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 11 + 5*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,10),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_false_b(V1,V2,Markers,1),
  write('trans(6,/ / /,6),'), nl,
  write('trans(6,$@(not_in([/]))/ $@(not_in([/])),7),'), nl,
  write('trans(7,/ / /,6),'), nl,
  write('trans(7,$@(not_in([/]))/ $@(not_in([/])),7),'), nl,
  write('trans(9,''|''/''|'',6),'), nl,
  write('trans(9,$@(not_in([]))/ $@(not_in([])),9),'), nl,
  write('trans(10,'),write(C1),write(' / '),write(C3),write(',9)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2!=mt, v1 and v2 point to the same list item,
% but v1 precedes v2 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_false_c(_,_,[],_) :- !.

gen_branches_if_V1_next_V2_false_c(V1,V2,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S4=4, S7=7);
    (S2 is 9 + (I-2)*4 + 1,
     S3 is 9 + (I-2)*4 + 2,
     S4 is 9 + (I-2)*4 + 3,
     S7 is 9 + (I-2)*4 + 4) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V2),write(' / '),write(V2),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans(5,'),write(V1),write(' / '),write(V1),write(','),write(S4),write('),'), nl,
  write('trans(5,$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(','),write(V1),write(' / '),write(V1),write(','),write(S4),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S7),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_false_c(V1,V2,Markers,II).

%...............................................................................

gen_if_V1_next_V2_false_c(FOut,C1,C3,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + 4*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_false_c(V1,V2,Markers,1),
  write('trans(5,/ / /,5),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),6),'), nl,
  write('trans(6,/ / /,5),'), nl,
  write('trans(6,$@(not_in([/]))/ $@(not_in([/])),6),'), nl,
  write('trans(8,''|''/''|'',5),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,'),write(C1),write(' / '),write(C3),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2!=mt, v1 and v2 point to the same list item,
% but v2 precedes v1 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_false_d(_,_,[],_) :- !.

gen_branches_if_V1_next_V2_false_d(V1,V2,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S4=4, S7=7);
    (S2 is 9 + (I-2)*4 + 1,
     S3 is 9 + (I-2)*4 + 2,
     S4 is 9 + (I-2)*4 + 3,
     S7 is 9 + (I-2)*4 + 4) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' / '),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans(5,'),write(V2),write(' / '),write(V2),write(','),write(S4),write('),'), nl,
  write('trans(5,$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(','),write(V2),write(' / '),write(V2),write(','),write(S4),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S7),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_false_d(V1,V2,Markers,II).

%...............................................................................

gen_if_V1_next_V2_false_d(FOut,C1,C3,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + 4*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_false_d(V1,V2,Markers,1),
  write('trans(5,/ / /,5),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),6),'), nl,
  write('trans(6,/ / /,5),'), nl,
  write('trans(6,$@(not_in([/]))/ $@(not_in([/])),6),'), nl,
  write('trans(8,''|''/''|'',5),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,'),write(C1),write(' / '),write(C3),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The situation of v1->mf, v2!=mt, v1 and v2 point to the same list item,
% and they are in fact the same variable.
%-------------------------------------------------------------------------------

gen_branches_if_V1_next_V2_false_e(_,[],_) :- !.

gen_branches_if_V1_next_V2_false_e(V12,[Marker|Markers],I) :-
  ( (I=1, !, S2=2, S3=3, S6=6);
    (S2 is 8 + (I-2)*3 + 1,
     S3 is 8 + (I-2)*3 + 2,
     S6 is 8 + (I-2)*3 + 3) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans(4,'),write(V12),write(' / '),write(V12),write(','),write(S3),write('),'), nl,
  write('trans(4,$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S6),write('),'), nl,
  write('trans('),write(S6),write(','),write(V12),write(' / '),write(V12),write(','),write(S3),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S6),write('),'), nl,
  II is I+1,
  gen_branches_if_V1_next_V2_false_e(V12,Markers,II).

%...............................................................................

gen_if_V1_next_V2_false_e(FOut,C1,C3,V12,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 9 + 3*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_if_V1_next_V2_false_e(V12,Markers,1),
  write('trans(4,/ / /,4),'), nl,
  write('trans(4,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(5,/ / /,4),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(7,''|''/''|'',4),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,'),write(C1),write(' / '),write(C3),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The summarized command for v1 < v2
%-------------------------------------------------------------------------------

gen_if_V1_next_V2_for_V1_pred_V2(FOut,C1,C2,C3,V1,V2,Markers) :-
  gen_if_V1_next_V2_basic(':aux-0',C1,C2,C3,V1,V2,Markers),
  gen_if_V1_next_V2_true_a(':aux-1',C1,C2,V1,V2,Markers),
  gen_if_V1_next_V2_true_b(':aux-2',C1,C2,V1,V2,Markers),
  gen_if_V1_next_V2_true_c(':aux-3',C1,C2,V1,V2,Markers),
  gen_if_V1_next_V2_false_a(':aux-4',C1,C3,V1,V2,Markers),
  gen_if_V1_next_V2_false_b(':aux-5',C1,C3,V1,V2,Markers),
  gen_if_V1_next_V2_false_c(':aux-6',C1,C3,V1,V2,Markers),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3''),
                           file('':aux-4''),file('':aux-5''),file('':aux-6'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The summarized command for v2 > v1
%-------------------------------------------------------------------------------

gen_if_V1_next_V2_for_V2_pred_V1(FOut,C1,C2,C3,V1,V2,Markers) :-
  gen_if_V1_next_V2_basic(':aux-0',C1,C2,C3,V1,V2,Markers),
  gen_if_V1_next_V2_true_a(':aux-1',C1,C2,V1,V2,Markers),
  gen_if_V1_next_V2_true_b(':aux-2',C1,C2,V1,V2,Markers),
  gen_if_V1_next_V2_true_d(':aux-3',C1,C2,V1,V2,Markers),
  gen_if_V1_next_V2_false_a(':aux-4',C1,C3,V1,V2,Markers),
  gen_if_V1_next_V2_false_b(':aux-5',C1,C3,V1,V2,Markers),
  gen_if_V1_next_V2_false_d(':aux-6',C1,C3,V1,V2,Markers),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3''),
                           file('':aux-4''),file('':aux-5''),file('':aux-6'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The summarized command for v1 being equal to v2.
%-------------------------------------------------------------------------------

gen_if_V1_next_V2_for_V1_eq_V2(FOut,C1,C2,C3,V12,Markers) :-
  gen_if_V1_next_V2_basic_for_V1_eq_V2(':aux-0',C1,C2,C3,V12,Markers),
  gen_if_V1_next_V2_true_e(':aux-3',C1,C2,V12,Markers),
  gen_if_V1_next_V2_false_e(':aux-6',C1,C3,V12,Markers),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-3''),file('':aux-6'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (v1->next == v2) goto c2; else goto c3
% The final command.
%-------------------------------------------------------------------------------

gen_if_V1_next_V2(FOut,C1,C2,C3,V,V,Markers,ItemVarList) :-
  gen_if_V1_next_V2_for_V1_eq_V2(FOut,C1,C2,C3,V,Markers),
  !.
  
gen_if_V1_next_V2(FOut,C1,C2,C3,V1,V2,Markers,ItemVarList) :-
  is_v1v2(ItemVarList,V1,V2),
  gen_if_V1_next_V2_for_V1_pred_V2(FOut,C1,C2,C3,V1,V2,Markers),
  !.

gen_if_V1_next_V2(FOut,C1,C2,C3,V1,V2,Markers,ItemVarList) :-
  gen_if_V1_next_V2_for_V2_pred_V1(FOut,C1,C2,C3,V1,V2,Markers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: v1->next = v2; goto c2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The basic situations for v1 != v2.
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_basic(FOut,C1,C2,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('21,'), nl,
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
  write('trans(2,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(2,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(5,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write(' / '),write(C1),write(',2),'), nl,
  write('trans(7,'),write(C1),write(' / '),write(C2),write(',11),'), nl,
  write('trans(8,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(8,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),8),'), nl,
  write('trans(9,/ / /,8),'), nl,
  write('trans(9,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),9),'), nl,
  write('trans(10,'),write(V1),write(' / '),write(V1),write(',9),'), nl,
  write('trans(10,$@(not_in([]))/ $@(not_in([])),10),'), nl,
  write('trans(11,# / #,12),'), nl,
  write('trans(11,''|''/''|'',10),'), nl,
  write('trans(11,$@(not_in([]))/ $@(not_in([])),13),'), nl,
  write('trans(11,$@(not_in([#]))/ $@(not_in([#])),14),'), nl,
  write('trans(12,'),write(V2),write(' / '),write(V2),write(',20),'), nl,
  write('trans(12,$@(not_in([''|'']))/ $@(not_in([''|''])),12),'), nl,
  write('trans(13,''|''/''|'',10),'), nl,
  write('trans(13,$@(not_in([]))/ $@(not_in([])),13),'), nl,
  write('trans(14,# / #,12),'), nl,
  write('trans(14,$@(not_in([#]))/ $@(not_in([#])),14),'), nl,
  write('trans(15,!/ #,1),'), nl,
  write('trans(15,# / #,1),'), nl,
  write('trans(15,[]/ #,16),'), nl,
  write('trans(16,[]/''|'',17),'), nl,
  write('trans(17,$@(not_in([!,#,'), nl,
    gen_all_from_markers(Markers),
    write(']))/ $@(not_in([!,#,'), nl,
    gen_all_from_markers(Markers),
    write('])),1),'), nl,
  write('trans(18,/ / /,15),'), nl,
  write('trans(18,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),18),'), nl,
  write('trans(19,'),write(V1),write(' / '),write(V1),write(',18),'), nl,
  write('trans(19,$@(not_in([]))/ $@(not_in([])),19),'), nl,
  write('trans(20,''|''/''|'',19),'), nl,
  write('trans(20,$@(not_in([]))/ $@(not_in([])),20)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The basic situations for v1 = v2.
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_basic_for_V1_eq_V2(FOut,C1,V12) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('7,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,6),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,3),'), nl,
  write('trans(2,'),write(V12),write(' / '),write(V12),write(',1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V12),write(' / '),write(V12),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V12),write(' / '),write(V12),write(',1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write(' / '),write(C1),write(',2)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v2=# and v1/marker.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_a(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_a(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8);
    (S1 is 10 + (I-2)*8 + 1,
     S2 is 10 + (I-2)*8 + 2,
     S3 is 10 + (I-2)*8 + 3,
     S4 is 10 + (I-2)*8 + 4,
     S5 is 10 + (I-2)*8 + 5,
     S6 is 10 + (I-2)*8 + 6,
     S7 is 10 + (I-2)*8 + 7,
     S8 is 10 + (I-2)*8 + 8) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t/[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S1),write('),'), nl,
  write('trans('),write(S2),write(','),write(Marker),write('_f/ #,'),write(S1),write('),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(','),write(Marker),write('_t/[],'),write(S3),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(Marker),write('_t/[],'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' / '),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(','),write(V2),write(' / '),write(V2),write(','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([''|'']))/ $@(not_in([''|''])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',# / #,'),write(S5),write('),'), nl,
  write('trans('),write(S7),write(',/ / /,'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',[]/'),write(Marker),write('_f,'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mf(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(LtMarker),
    write('])),'),write(S8),write('),'), nl,
  write('trans(9,!/!,'),write(S8),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_a(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_V1_next_assign_V2_a(M,N) :-
  ( (M=1,!,F=1);
    (F is 11 + 8*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_V1_next_assign_V2_a(MM,N)) ).

%...............................................................................

gen_V1_next_assign_V2_a(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 11 + 8*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_V1_next_assign_V2_a(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,10),'), nl,
  gen_branches_V1_next_assign_V2_a(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(10,'),write(C1),write(' / '),write(C2),write(',9)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/marker, v2!=# (we redirect the marker), and the v1 list
% item precedes the v2 item.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_b(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_b(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6,S7=7);
    (S1 is 9 + (I-2)*7 + 1,
     S2 is 9 + (I-2)*7 + 2,
     S3 is 9 + (I-2)*7 + 3,
     S4 is 9 + (I-2)*7 + 4,
     S5 is 9 + (I-2)*7 + 5,
     S6 is 9 + (I-2)*7 + 6,
     S7 is 9 + (I-2)*7 + 7) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t/[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S1),write('),'), nl,
  write('trans('),write(S2),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S2),write('),'), nl,
  write('trans('),write(S2),write(','),write(V2),write(' / '),write(V2),write(','),write(S1),write('),'), nl,
  write('trans('),write(S3),write(',[]/'),write(Marker),write('_t,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S3),write('),'), nl,
  write('trans('),write(S3),write(','),write(Marker),write('_t / '),write(Marker),
    write('_t,'),write(S2),write('),'), nl,
  write('trans('),write(S4),write(','),write(Marker),write('_t/[],'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(','),write(Marker),write('_f/'),write(Marker),write('_f,'),
    write(S4),write('),'), nl,
  write('trans('),write(S6),write(',/ / /,'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(','),write(Marker),write('_t/[],'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(','),write(V1),write(' / '),write(V1),write(','),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S7),write('),'), nl,
  write('trans(8,''|''/''|'','),write(S7),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_b(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_V1_next_assign_V2_b(M,N) :-
  ( (M=1,!,F=1);
    (F is 10 + 7*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_V1_next_assign_V2_b(MM,N)) ).

%...............................................................................

gen_V1_next_assign_V2_b(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + 7*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_V1_next_assign_V2_b(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  gen_branches_V1_next_assign_V2_b(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(8,$@(not_in([''|'']))/ $@(not_in([''|''])),8),'), nl,
  write('trans(9,'),write(C1),write(' / '),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/marker, v2!=# (we redirect the marker), and the v2 list
% item precedes the v1 item.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_c(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_c(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9);
    (S1 is 11 + (I-2)*9 + 1,
     S2 is 11 + (I-2)*9 + 2,
     S3 is 11 + (I-2)*9 + 3,
     S4 is 11 + (I-2)*9 + 4,
     S5 is 11 + (I-2)*9 + 5,
     S6 is 11 + (I-2)*9 + 6,
     S7 is 11 + (I-2)*9 + 7,
     S8 is 11 + (I-2)*9 + 8,
     S9 is 11 + (I-2)*9 + 9) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t/[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S1),write('),'), nl,
  write('trans('),write(S2),write(','),write(Marker),write('_f/'),write(Marker),write('_f,'),
    write(S1),write('),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),
    write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(Marker),write('_t/[],'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' / '),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(','),write(Marker),write('_t/[],'),write(S5),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S6),write('),'), nl,
  write('trans('),write(S6),write(','),write(V2),write(' / '),write(V2),write(','),write(S5),write('),'), nl,
  write('trans('),write(S7),write(',[]/'),write(Marker),write('_t,'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S8),write('),'), nl,
  write('trans('),write(S7),write(','),write(Marker),write('_t/[],'),write(S9),write('),'), nl,
  write('trans('),write(S7),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S8),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S9),write('),'), nl,
  write('trans('),write(S8),write(',[]/'),write(Marker),write('_t,'),write(S6),write('),'), nl,
  write('trans('),write(S8),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S8),write('),'), nl,
  write('trans('),write(S8),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S6),write('),'), nl,
  write('trans('),write(S9),write(','),write(Marker),write('_t/[],'),write(S9),write('),'), nl,
  write('trans('),write(S9),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S8),write('),'), nl,
  write('trans('),write(S9),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S9),write('),'), nl,
  write('trans(10,''|''/''|'','),write(S7),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_c(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_V1_next_assign_V2_c(M,N) :-
  ( (M=1,!,F=1);
    (F is 12 + 9*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_V1_next_assign_V2_c(MM,N)) ).

%...............................................................................

gen_V1_next_assign_V2_c(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 12 + 9*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_V1_next_assign_V2_c(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,11),'), nl,
  gen_branches_V1_next_assign_V2_c(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(10,$@(not_in([''|'']))/ $@(not_in([''|''])),10),'), nl,
  write('trans(11,'),write(C1),write(' / '),write(C2),write(',10)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/marker, v2!=# (we redirect the marker), v1 and v2 point to
% the same list item, but v1 precedes v2 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_d(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_d(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8);
    (S1 is 10 + (I-2)*8 + 1,
     S2 is 10 + (I-2)*8 + 2,
     S3 is 10 + (I-2)*8 + 3,
     S4 is 10 + (I-2)*8 + 4,
     S5 is 10 + (I-2)*8 + 5,
     S6 is 10 + (I-2)*8 + 6,
     S7 is 10 + (I-2)*8 + 7,
     S8 is 10 + (I-2)*8 + 8) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t/[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S1),write('),'), nl,
  write('trans('),write(S2),write(','),write(Marker),write('_f/'),write(Marker),write('_f,'),
    write(S1),write('),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V2),write(' / '),write(V2),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S5),write('),'), nl,
  write('trans('),write(S5),write(','),write(V1),write(' / '),write(V1),write(','),write(S4),write('),'), nl,
  write('trans('),write(S6),write(',[]/'),write(Marker),write('_t,'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S6),write(','),write(Marker),write('_t/[],'),write(S8),write('),'), nl,
  write('trans('),write(S6),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S7),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S8),write('),'), nl,
  write('trans('),write(S7),write(',[]/'),write(Marker),write('_t,'),write(S5),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S5),write('),'), nl,
  write('trans('),write(S8),write(','),write(Marker),write('_t/[],'),write(S8),write('),'), nl,
  write('trans('),write(S8),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S8),write('),'), nl,
  write('trans(9,''|''/''|'','),write(S6),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_d(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_V1_next_assign_V2_d(M,N) :-
  ( (M=1,!,F=1);
    (F is 11 + 8*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_V1_next_assign_V2_d(MM,N)) ).

%...............................................................................

gen_V1_next_assign_V2_d(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 11 + 8*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_V1_next_assign_V2_d(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,10),'), nl,
  gen_branches_V1_next_assign_V2_d(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(9,$@(not_in([''|'']))/ $@(not_in([''|''])),9),'), nl,
  write('trans(10,'),write(C1),write(' / '),write(C2),write(',9)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/marker, v2!=# (we redirect the marker), v1 and v2 point to
% the same list item, but v2 precedes v1 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_e(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_e(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8);
    (S1 is 10 + (I-2)*8 + 1,
     S2 is 10 + (I-2)*8 + 2,
     S3 is 10 + (I-2)*8 + 3,
     S4 is 10 + (I-2)*8 + 4,
     S5 is 10 + (I-2)*8 + 5,
     S6 is 10 + (I-2)*8 + 6,
     S7 is 10 + (I-2)*8 + 7,
     S8 is 10 + (I-2)*8 + 8) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t/[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S1),write('),'), nl,
  write('trans('),write(S2),write(','),write(Marker),write('_f/'),write(Marker),write('_f,'),
    write(S1),write('),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' / '),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S5),write('),'), nl,
  write('trans('),write(S5),write(','),write(V2),write(' / '),write(V2),write(','),write(S4),write('),'), nl,
  write('trans('),write(S6),write(',[]/'),write(Marker),write('_t,'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S6),write(','),write(Marker),write('_t/[],'),write(S8),write('),'), nl,
  write('trans('),write(S6),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S7),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S8),write('),'), nl,
  write('trans('),write(S7),write(',[]/'),write(Marker),write('_t,'),write(S5),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S5),write('),'), nl,
  write('trans('),write(S8),write(','),write(Marker),write('_t/[],'),write(S8),write('),'), nl,
  write('trans('),write(S8),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S8),write('),'), nl,
  write('trans(9,''|''/''|'','),write(S6),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_e(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_V1_next_assign_V2_e(M,N) :-
  ( (M=1,!,F=1);
    (F is 11 + 8*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_V1_next_assign_V2_e(MM,N)) ).

%...............................................................................

gen_V1_next_assign_V2_e(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 11 + 8*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_V1_next_assign_V2_e(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,10),'), nl,
  gen_branches_V1_next_assign_V2_e(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(9,$@(not_in([''|'']))/ $@(not_in([''|''])),9),'), nl,
  write('trans(10,'),write(C1),write(' / '),write(C2),write(',9)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/marker, v2!=# (we redirect the marker), v1 and v2 point to
% the same list item, and they are in fact the same variable.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_f(_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_f(V12,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6,S7=7);
    (S1 is 9 + (I-2)*7 + 1,
     S2 is 9 + (I-2)*7 + 2,
     S3 is 9 + (I-2)*7 + 3,
     S4 is 9 + (I-2)*7 + 4,
     S5 is 9 + (I-2)*7 + 5,
     S6 is 9 + (I-2)*7 + 6,
     S7 is 9 + (I-2)*7 + 7) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t/[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S1),write('),'), nl,
  write('trans('),write(S2),write(','),write(Marker),write('_f/'),write(Marker),write('_f,'),
    write(S1),write('),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(','),write(V12),write(' / '),write(V12),write(','),write(S3),write('),'), nl,
  write('trans('),write(S5),write(',[]/'),write(Marker),write('_t,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S6),write('),'), nl,
  write('trans('),write(S5),write(','),write(Marker),write('_t/[],'),write(S7),write('),'), nl,
  write('trans('),write(S5),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S6),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S7),write('),'), nl,
  write('trans('),write(S6),write(',[]/'),write(Marker),write('_t,'),write(S4),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S6),write('),'), nl,
  write('trans('),write(S6),write(','),write(Marker),write('_t/'),write(Marker),write('_t,'),
    write(S4),write('),'), nl,
  write('trans('),write(S7),write(','),write(Marker),write('_t/[],'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),
    write(Marker),write('_t])),'),write(S7),write('),'), nl,
  write('trans(8,''|''/''|'','),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_f(V12,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_V1_next_assign_V2_f(M,N) :-
  ( (M=1,!,F=1);
    (F is 10 + 7*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_V1_next_assign_V2_f(MM,N)) ).

%...............................................................................

gen_V1_next_assign_V2_f(FOut,C1,C2,V12,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + 7*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_V1_next_assign_V2_f(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  gen_branches_V1_next_assign_V2_f(V12,Markers,1,Markers,ItemVarList),
  write('trans(8,$@(not_in([''|'']))/ $@(not_in([''|''])),8),'), nl,
  write('trans(9,'),write(C1),write(' / '),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/ not marker, v2!=# (we use a fresh marker), and the v1 list
% item precedes the v2 item.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_g(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_g(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9,S10=10);
    (S2 is 11 + (I-2)*9 + 2,
     S3 is 11 + (I-2)*9 + 3,
     S4 is 11 + (I-2)*9 + 4,
     S5 is 11 + (I-2)*9 + 5,
     S6 is 11 + (I-2)*9 + 6,
     S7 is 11 + (I-2)*9 + 7,
     S8 is 11 + (I-2)*9 + 8,
     S9 is 11 + (I-2)*9 + 9,
    S10 is 11 + (I-2)*9 + 10) ),
  write('trans('),write(S2),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S2),write('),'), nl,
  write('trans('),write(S2),write(','),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans('),write(S3),write(',[]/'),write(Marker),write('_t,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',[]/'),write(Marker),write('_f,'),write(S6),write('),'), nl,
  write('trans('),write(S5),write(',in([!,#])/'),write(Marker),write('_f,'),write(S4),write('),'), nl,
  write('trans('),write(S6),write(',[]/''|'','),write(S7),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write(']))/ $@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S8),write(',/ / /,'),write(S5),write('),'), nl,
  write('trans('),write(S8),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S8),write('),'), nl,
  write('trans('),write(S9),write(','),write(V1),write(' / '),write(V1),write(','),write(S8),write('),'), nl,
  write('trans('),write(S9),write(',$@(not_in([]))/ $@(not_in([])),'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',''|''/''|'','),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',$@(not_in([]))/ $@(not_in([])),'),write(S10),write('),'), nl,
  write('trans(11,'),write(Marker),write('_f/[],'),write(S10),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_g(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_next_assign_V2_g(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 13 + 9*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,12),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_V1_next_assign_V2_g(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(11,$@(not_in([/]))/ $@(not_in([/])),11),'), nl,
  write('trans(12,'),write(C1),write(' / '),write(C2),write(',11)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/ not marker, v2!=# (we use a fresh marker), and the v2 list
% item precedes the v1 item.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_h(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_h(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9,S10=10,S11=11,S12=12);
    (S2 is 13 + (I-2)*11 + 2,
     S3 is 13 + (I-2)*11 + 3,
     S4 is 13 + (I-2)*11 + 4,
     S5 is 13 + (I-2)*11 + 5,
     S6 is 13 + (I-2)*11 + 6,
     S7 is 13 + (I-2)*11 + 7,
     S8 is 13 + (I-2)*11 + 8,
     S9 is 13 + (I-2)*11 + 9,
    S10 is 13 + (I-2)*11 + 10,
    S11 is 13 + (I-2)*11 + 11,
    S12 is 13 + (I-2)*11 + 12) ),
  write('trans('),write(S2),write(',[]/'),write(Marker),write('_f,'),write(S3),write('),'), nl,
  write('trans('),write(S2),write(',in([!,#])/'),write(Marker),write('_f,1),'), nl,
  write('trans('),write(S3),write(',[]/''|'','),write(S4),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write(']))/ $@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write('])),1),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(','),write(V1),write(' / '),write(V1),write(','),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in([]))/ $@(not_in([])),'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',/ / /,'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in([]))/ $@(not_in([])),'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S8),write('),'), nl,
  write('trans('),write(S8),write(','),write(V2),write(' / '),write(V2),write(','),write(S7),write('),'), nl,
  write('trans('),write(S9),write(',/ / /,'),write(S9),write('),'), nl,
  write('trans('),write(S9),write(',[]/'),write(Marker),write('_t,'),write(S8),write('),'), nl,
  write('trans('),write(S9),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S10),write('),'), nl,
  write('trans('),write(S9),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S11),write('),'), nl,
  write('trans('),write(S10),write(',[]/'),write(Marker),write('_t,'),write(S8),write('),'), nl,
  write('trans('),write(S10),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S10),write('),'), nl,
  write('trans('),write(S11),write(',/ / /,'),write(S9),write('),'), nl,
  write('trans('),write(S11),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S11),write('),'), nl,
  write('trans('),write(S12),write(',''|''/''|'','),write(S9),write('),'), nl,
  write('trans('),write(S12),write(',$@(not_in([]))/ $@(not_in([])),'),write(S12),write('),'), nl,
  write('trans(13,'),write(Marker),write('_f/[],'),write(S12),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_h(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_next_assign_V2_h(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 15 + 11*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,14),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_V1_next_assign_V2_h(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(13,$@(not_in([/]))/ $@(not_in([/])),13),'), nl,
  write('trans(14,'),write(C1),write(' / '),write(C2),write(',13)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/ not marker, v2!=# (we use a fresh marker), v1 and v2 point to
% the same list item, but v1 precedes v2 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_i(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_i(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9,S10=10,S11=11);
    (S2 is 12 + (I-2)*10 + 2,
     S3 is 12 + (I-2)*10 + 3,
     S4 is 12 + (I-2)*10 + 4,
     S5 is 12 + (I-2)*10 + 5,
     S6 is 12 + (I-2)*10 + 6,
     S7 is 12 + (I-2)*10 + 7,
     S8 is 12 + (I-2)*10 + 8,
     S9 is 12 + (I-2)*10 + 9,
    S10 is 12 + (I-2)*10 + 10,
    S11 is 12 + (I-2)*10 + 11) ),
  write('trans('),write(S2),write(',[]/'),write(Marker),write('_f,'),write(S3),write('),'), nl,
  write('trans('),write(S2),write(',in([!,#])/'),write(Marker),write('_f,1),'), nl,
  write('trans('),write(S3),write(',[]/''|'','),write(S4),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write(']))/ $@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write('])),1),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(','),write(V2),write(' / '),write(V2),write(','),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(','),write(V1),write(' / '),write(V1),write(','),write(S6),write('),'), nl,
  write('trans('),write(S8),write(',/ / /,'),write(S8),write('),'), nl,
  write('trans('),write(S8),write(',[]/'),write(Marker),write('_t,'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S9),write('),'), nl,
  write('trans('),write(S8),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S10),write('),'), nl,
  write('trans('),write(S9),write(',[]/'),write(Marker),write('_t,'),write(S7),write('),'), nl,
  write('trans('),write(S9),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',/ / /,'),write(S8),write('),'), nl,
  write('trans('),write(S10),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S10),write('),'), nl,
  write('trans('),write(S11),write(',''|''/''|'','),write(S8),write('),'), nl,
  write('trans('),write(S11),write(',$@(not_in([]))/ $@(not_in([])),'),write(S11),write('),'), nl,
  write('trans(12,'),write(Marker),write('_f/[],'),write(S11),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_i(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_next_assign_V2_i(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 14 + 10*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,13),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_V1_next_assign_V2_i(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(12,$@(not_in([/]))/ $@(not_in([/])),12),'), nl,
  write('trans(13,'),write(C1),write(' / '),write(C2),write(',12)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/ not marker, v2!=# (we use a fresh marker), v1 and v2 point to
% the same list item, but v2 precedes v1 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_j(_,_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_j(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9,S10=10,S11=11);
    (S2 is 12 + (I-2)*10 + 2,
     S3 is 12 + (I-2)*10 + 3,
     S4 is 12 + (I-2)*10 + 4,
     S5 is 12 + (I-2)*10 + 5,
     S6 is 12 + (I-2)*10 + 6,
     S7 is 12 + (I-2)*10 + 7,
     S8 is 12 + (I-2)*10 + 8,
     S9 is 12 + (I-2)*10 + 9,
    S10 is 12 + (I-2)*10 + 10,
    S11 is 12 + (I-2)*10 + 11) ),
  write('trans('),write(S2),write(',[]/'),write(Marker),write('_f,'),write(S3),write('),'), nl,
  write('trans('),write(S2),write(',in([!,#])/'),write(Marker),write('_f,1),'), nl,
  write('trans('),write(S3),write(',[]/''|'','),write(S4),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write(']))/ $@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write('])),1),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(','),write(V1),write(' / '),write(V1),write(','),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(','),write(V2),write(' / '),write(V2),write(','),write(S6),write('),'), nl,
  write('trans('),write(S8),write(',/ / /,'),write(S8),write('),'), nl,
  write('trans('),write(S8),write(',[]/'),write(Marker),write('_t,'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S9),write('),'), nl,
  write('trans('),write(S8),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S10),write('),'), nl,
  write('trans('),write(S9),write(',[]/'),write(Marker),write('_t,'),write(S7),write('),'), nl,
  write('trans('),write(S9),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',/ / /,'),write(S8),write('),'), nl,
  write('trans('),write(S10),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S10),write('),'), nl,
  write('trans('),write(S11),write(',''|''/''|'','),write(S8),write('),'), nl,
  write('trans('),write(S11),write(',$@(not_in([]))/ $@(not_in([])),'),write(S11),write('),'), nl,
  write('trans(12,'),write(Marker),write('_f/[],'),write(S11),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_j(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_next_assign_V2_j(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 14 + 10*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,13),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_V1_next_assign_V2_j(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(12,$@(not_in([/]))/ $@(not_in([/])),12),'), nl,
  write('trans(13,'),write(C1),write(' / '),write(C2),write(',12)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation of v1/ not marker, v2!=# (we use a fresh marker), v1 and v2 point to
% the same list item, and they are in fact the same variable.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_V2_k(_,[],_,_,_) :- !.

gen_branches_V1_next_assign_V2_k(V12,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9,S10=10);
    (S2 is 11 + (I-2)*9 + 2,
     S3 is 11 + (I-2)*9 + 3,
     S4 is 11 + (I-2)*9 + 4,
     S5 is 11 + (I-2)*9 + 5,
     S6 is 11 + (I-2)*9 + 6,
     S7 is 11 + (I-2)*9 + 7,
     S8 is 11 + (I-2)*9 + 8,
     S9 is 11 + (I-2)*9 + 9,
    S10 is 11 + (I-2)*9 + 10) ),
  write('trans('),write(S2),write(',[]/'),write(Marker),write('_f,'),write(S3),write('),'), nl,
  write('trans('),write(S2),write(',in([!,#])/'),write(Marker),write('_f,1),'), nl,
  write('trans('),write(S3),write(',[]/''|'','),write(S4),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write(']))/ $@(not_in([!,#,'), nl,
    gen_all_from_markers(AllMarkers),
    write('])),1),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S6),write('),'), nl,
  write('trans('),write(S6),write(','),write(V12),write(' / '),write(V12),write(','),write(S5),write('),'), nl,
  write('trans('),write(S7),write(',/ / /,'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(',[]/'),write(Marker),write('_t,'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S8),write('),'), nl,
  write('trans('),write(S7),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S9),write('),'), nl,
  write('trans('),write(S8),write(',[]/'),write(Marker),write('_t,'),write(S6),write('),'), nl,
  write('trans('),write(S8),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mt(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(LtMarker),
    write('])),'),write(S8),write('),'), nl,
  write('trans('),write(S9),write(',/ / /,'),write(S7),write('),'), nl,
  write('trans('),write(S9),write(',$@(not_in([/]))/ $@(not_in([/])),'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',''|''/''|'','),write(S7),write('),'), nl,
  write('trans('),write(S10),write(',$@(not_in([]))/ $@(not_in([])),'),write(S10),write('),'), nl,
  write('trans(11,'),write(Marker),write('_f/[],'),write(S10),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_V2_k(V12,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_next_assign_V2_k(FOut,C1,C2,V12,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 13 + 9*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,12),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_V1_next_assign_V2_k(V12,Markers,1,Markers,ItemVarList),
  write('trans(11,$@(not_in([/]))/ $@(not_in([/])),11),'), nl,
  write('trans(12,'),write(C1),write(' / '),write(C2),write(',11)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The situation when we need a marker and all are in use: in the next step, we 
% either go to unknown or start shifting.
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_shift_or_unknown(FOut,C1,V1,V2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('7,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/su,6),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,$@(in([/,''|'']))/ $@(in([/,''|''])),1),'), nl,
  write('trans(2,$@(not_in(['), nl,
    gen_all_from_markers(Markers),
    write(','),write(V2),write(']))/ $@(not_in(['), nl,
    gen_all_from_markers(Markers),
    write(','),write(V2),write('])),2),'), nl,
  write('trans(3,/ / /,2),'), nl,
  write('trans(3,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),3),'), nl,
  write('trans(4,'),write(V1),write(' / '),write(V1),write(',3),'), nl,
  write('trans(4,$@(not_in([]))/ $@(not_in([])),4),'), nl,
  write('trans(5,''|''/''|'',4),'), nl,
  write('trans(5,$@(not_in(['), nl,
    gen_all_from_markers(Markers),
    write(','),write(V2),write(']))/ $@(not_in(['), nl,
    gen_all_from_markers(Markers),
    write(','),write(V2),write('])),5),'), nl,
  write('trans(6,'),write(C1),write(' / '),write(C1),write(',5)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% We need to shift, we can, and so we start shifting - the situation of mark_f
% followed by mark_t.
%-------------------------------------------------------------------------------

gen_branches_start_shifting_a([],_,_,_) :- !.

gen_branches_start_shifting_a([Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9,S10=10);
    (S2 is 11 + (I-2)*9 + 2,
     S3 is 11 + (I-2)*9 + 3,
     S4 is 11 + (I-2)*9 + 4,
     S5 is 11 + (I-2)*9 + 5,
     S6 is 11 + (I-2)*9 + 6,
     S7 is 11 + (I-2)*9 + 7,
     S8 is 11 + (I-2)*9 + 8,
     S9 is 11 + (I-2)*9 + 9,
    S10 is 11 + (I-2)*9 + 10) ),
  write('trans('),write(S2),write(','),write(Marker),write('_t/[],1),'), nl,
  write('trans('),write(S2),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',[]/ /,'),write(S2),write('),'), nl,
  write('trans('),write(S4),write(',[]/''_t'','),write(S3),write('),'), nl,
  write('trans('),write(S5),write(',''|''/''|'','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([]))/ $@(not_in([])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',''|''/[],'),write(S5),write('),'), nl,
  write('trans('),write(S7),write(','),write(Marker),write('_f/[],'),write(S6),write('),'), nl,
  write('trans('),write(S8),write(',/ /''_f'','),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(not_in([]))/ $@(not_in([])),'),write(S8),write('),'), nl,
  write('trans('),write(S9),write(',/ / /,'),write(S8),write('),'), nl,
  write('trans('),write(S9),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',[]/'),write(Marker),write('_f,'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mf(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(LtMarker),
    write('])),'),write(S10),write('),'), nl,
  write('trans(11,!/!,'),write(S10),write('),'), nl,
  II is I+1,
  gen_branches_start_shifting_a(Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_start_shifting_a(FOut,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 13 + 9*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,su/s,12),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_start_shifting_a(Markers,1,Markers,ItemVarList),
  write('trans(12,$@(not_in([]))/ $@(not_in([])),11)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% We need to shift, we can, and so we start shifting - the situation of mark_t
% followed by mark_f.
%-------------------------------------------------------------------------------

gen_branches_start_shifting_b([],_,_,_) :- !.

gen_branches_start_shifting_b([Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S3=3,S4=4,S5=5,S6=6,S7=7,S8=8,S9=9,S10=10,S11=11);
    (S3 is 11 + (I-2)*9 + 3,
     S4 is 11 + (I-2)*9 + 4,
     S5 is 11 + (I-2)*9 + 5,
     S6 is 11 + (I-2)*9 + 6,
     S7 is 11 + (I-2)*9 + 7,
     S8 is 11 + (I-2)*9 + 8,
     S9 is 11 + (I-2)*9 + 9,
    S10 is 11 + (I-2)*9 + 10,
    S11 is 11 + (I-2)*9 + 11) ),
  write('trans('),write(S3),write(','),write(Marker),write('_f/[],2),'), nl,
  write('trans('),write(S4),write(',/ /''_f'','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',''|''/''|'','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([]))/ $@(not_in([])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(','),write(Marker),write('_t/[],'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',[]/ /,'),write(S6),write('),'), nl,
  write('trans('),write(S8),write(',[]/''_t'','),write(S7),write('),'), nl,
  write('trans('),write(S9),write(',''|''/''|'','),write(S8),write('),'), nl,
  write('trans('),write(S9),write(',$@(not_in([]))/ $@(not_in([])),'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',/ / /,'),write(S9),write('),'), nl,
  write('trans('),write(S10),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S10),write('),'), nl,
  write('trans('),write(S11),write(',[]/'),write(Marker),write('_f,'),write(S10),write('),'), nl,
  write('trans('),write(S11),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mf(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(LtMarker),
    write('])),'),write(S11),write('),'), nl,
  write('trans(12,!/!,'),write(S11),write('),'), nl,
  II is I+1,
  gen_branches_start_shifting_b(Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_start_shifting_b(FOut,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 14 + 9*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,su/s,13),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,''|''/[],1),'), nl,
  gen_branches_start_shifting_b(Markers,1,Markers,ItemVarList),
  write('trans(13,$@(not_in([]))/ $@(not_in([])),12)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% We need to shift, but we cannot (no marker can be released), and so we go to
% unknown.
%-------------------------------------------------------------------------------

gen_branches_to_unknown([],_) :- !.

gen_branches_to_unknown([Marker|Markers],I) :-
  ( (I=1, !, S3=3,S4=4);
    (S3 is 2 + (I-2)*2 + 3,
     S4 is 2 + (I-2)*2 + 4) ),
  write('trans(1,'),write(Marker),write('_t/'),write(Marker),write('_t,'),write(S3),write('),'), nl,
  write('trans('),write(S3),write(','),write(Marker),write('_f/'),
    write(Marker),write('_f,'),write(S4),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in(['),write(Marker),write('_f,''|'']))/ $@(not_in(['),
    write(Marker),write('_f,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',''|''/''|'',1),'), nl,
  II is I+1,
  gen_branches_to_unknown(Markers,II).

%...............................................................................

gen_to_unknown(FOut,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 5 + 2*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,su/u,2),'), nl,
  write('trans(1,/ / /,2),'), nl,
  gen_branches_to_unknown(Markers,1),
  write('trans(1,$@(not_in([/,'), nl,
    gen_all_to_markers(Markers),
    write(',''|'']))/ $@(not_in([/,'), nl,
    gen_all_to_markers(Markers),
    write(',''|''])),1),'), nl,
  write('trans(2,''|''/''|'',1),'), nl,
  write('trans(2,$@(not_in([''|'']))/ $@(not_in([''|''])),2)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The shifting itself. ( We do not shift from_markers - that's not necessary.
%-------------------------------------------------------------------------------

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

%...............................................................................

gen_shift_to_markers([],_,_) :- !.

gen_shift_to_markers([A|Markers],NItemVar,I) :-
  S2 is 9 + NItemVar*6 + I*6 + 1,
  S3 is 9 + NItemVar*6 + I*6 + 2,
  S4 is 9 + NItemVar*6 + I*6 + 3,
  S5 is 9 + NItemVar*6 + I*6 + 4,
  S7 is 9 + NItemVar*6 + I*6 + 5,
  S9 is 9 + NItemVar*6 + I*6 + 6,
  write('trans('),write(S2),write(',''_f''/''_f'',1),'), nl,
  write('trans('),write(S3),write(',[]/ '),write(A),write('_t,1),'), nl,
  write('trans('),write(S4),write(','),write(A),write('_t /[],'),write(S2),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',[]/ '),write(A),write('_t,'),write(S4),write('),'), nl,
  write('trans('),write(6),write(',''_t''/''_t'','),write(S5),write('),'), nl,
  write('trans('),write(6),write(','),write(A),write('_t /[],'),write(S7),write('),'), nl,
  write('trans('),write(S7),write(',''_f''/''_f'','),write(S9),write('),'), nl,
  write('trans('),write(S9),write(',''_t''/''_t'','),write(S3),write('),'), nl,
  write('trans('),write(S9),write(',$@(not_in([]))/ $@(not_in([])),'),write(S9),write('),'), nl,
  II is I+1,
  gen_shift_to_markers(Markers,NItemVar,II).

%...............................................................................

gen_V1_next_assign_V2_do_shifting(FOut,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  length(ItemVarList,Len1),
  length(Markers,Len2),
  N is 10 + (Len1 + Len2) * 6,
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
  gen_shift_to_markers(Markers,Len1,0),
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
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The end of the shifting phase.
%-------------------------------------------------------------------------------

gen_end_shifting(FOut) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('8,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,s/n,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,''_f''/[],1),'), nl,
  write('trans(3,''_t''/[],1),'), nl,
  write('trans(3,$@(not_in([]))/ $@(not_in([])),3),'), nl,
  write('trans(4,''|''/''|'',2),'), nl,
  write('trans(4,$@(not_in([]))/ $@(not_in([])),4),'), nl,
  write('trans(5,''_t''/[],4),'), nl,
  write('trans(5,''|''/''|'',6),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(6,''_f''/[],3),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),5)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The summarized command for v1 < v2.
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_for_V1_pred_V2(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  gen_V1_next_assign_V2_basic(':aux-0',C1,C2,V1,V2,Markers),
  gen_V1_next_assign_V2_a(':aux-1',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_b(':aux-2',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_c(':aux-3',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_d(':aux-4',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_g(':aux-5',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_h(':aux-6',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_i(':aux-7',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_shift_or_unknown(':aux-8',C1,V1,V2,Markers),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3''),
    file('':aux-4''),file('':aux-5''),file('':aux-6''),file('':aux-7''),file('':aux-8'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The summarized command for v2 < v1.
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_for_V2_pred_V1(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  gen_V1_next_assign_V2_basic(':aux-0',C1,C2,V1,V2,Markers),
  gen_V1_next_assign_V2_a(':aux-1',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_b(':aux-2',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_c(':aux-3',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_e(':aux-4',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_g(':aux-5',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_h(':aux-6',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_j(':aux-7',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_shift_or_unknown(':aux-8',C1,V1,V2,Markers),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3''),
    file('':aux-4''),file('':aux-5''),file('':aux-6''),file('':aux-7''),file('':aux-8'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The summarized command for v1 = v2.
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_for_V1_eq_v2(FOut,C1,C2,V12,Markers,ItemVarList) :-
  gen_V1_next_assign_V2_basic_for_V1_eq_V2(':aux-0',C1,V12),
  gen_V1_next_assign_V2_f(':aux-1',C1,C2,V12,Markers,ItemVarList),
  gen_V1_next_assign_V2_k(':aux-2',C1,C2,V12,Markers,ItemVarList),
  gen_V1_next_assign_V2_shift_or_unknown(':aux-3',C1,V12,V12,Markers),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The summarized command for shifting.
%-------------------------------------------------------------------------------

gen_V1_next_assign_V2_shifting(FOut,Markers,ItemVarList) :-
  gen_start_shifting_a(':aux-1',Markers,ItemVarList),
  gen_start_shifting_b(':aux-2',Markers,ItemVarList),
  gen_to_unknown(':aux-3',Markers),
  gen_V1_next_assign_V2_do_shifting(':aux-4',Markers,ItemVarList),
  gen_end_shifting(':aux-5'),
  fsa_regex_atom_compile('{file('':aux-1''),file('':aux-2''),file('':aux-3''),
    file('':aux-4''),file('':aux-5'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = v2; goto c2;
% The final command.
%-------------------------------------------------------------------------------

%% May be optimized by separating the generation of the basis and the shifting,
%% which is always the same - here, it is as follows in order to make sure
%% the shifting is never omitted.

gen_V1_next_assign_V2(FOut1,FOut2,C1,C2,V,V,Markers,ItemVarList) :-
  gen_V1_next_assign_V2_for_V1_eq_v2(FOut1,C1,C2,V,Markers,ItemVarList),
  gen_V1_next_assign_V2_shifting(FOut2,Markers,ItemVarList),
  !.
  
gen_V1_next_assign_V2(FOut1,FOut2,C1,C2,V1,V2,Markers,ItemVarList) :-
  is_v1v2(ItemVarList,V1,V2),
  gen_V1_next_assign_V2_for_V1_pred_V2(FOut1,C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_shifting(FOut2,Markers,ItemVarList),
  !.

gen_V1_next_assign_V2(FOut1,FOut2,C1,C2,V1,V2,Markers,ItemVarList) :-
  gen_V1_next_assign_V2_for_V2_pred_V1(FOut1,C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_next_assign_V2_shifting(FOut2,Markers,ItemVarList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: v1 = v2->next; goto c2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The basic situations.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_basic(FOut,C1,V2) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('11,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1,'), nl,
  write('2'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,10),'), nl,
  write('trans(1,/ / /,3),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(1,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),4),'), nl,
  write('trans(2,$@(not_in([]))/ $@(not_in([])),2),'), nl,
  write('trans(3,!/!,2),'), nl,
  write('trans(4,/ / /,3),'), nl,
  write('trans(4,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),4),'), nl,
  write('trans(5,# / #,6),'), nl,
  write('trans(5,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(5,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(6,'),write(V2),write(' / '),write(V2),write(',2),'), nl,
  write('trans(6,$@(not_in([''|'']))/ $@(not_in([''|''])),6),'), nl,
  write('trans(7,'),write(V2),write(' / '),write(V2),write(',4),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,# / #,6),'), nl,
  write('trans(8,$@(not_in([#]))/ $@(not_in([#])),8),'), nl,
  write('trans(9,'),write(V2),write(' / '),write(V2),write(',2),'), nl,
  write('trans(9,$@(not_in([/]))/ $@(not_in([/])),9),'), nl,
  write('trans(10,'),write(C1),write(' / '),write(C1),write(',5)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=# and v1!=v2.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_a(FOut,C1,C2,V1,V2,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('9,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  write('trans(2,# / #,1),'), nl,
  write('trans(3,/ / /,2),'), nl,
  write('trans(3,'),write(V1),write('/[],3),'), nl,
  write('trans(3,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),3),'), nl,
  write('trans(4,'),write(V1),write('/[],4),'), nl,
  write('trans(4,'),write(V2),write('/'),write(V2),write(',3),'), nl,
  write('trans(4,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),4),'), nl,
  write('trans(5,$@(in(['),
    gtv1(ItemVarList,V1,GtV1),
    gen_comma_sep_list(GtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV1),
    write('])),5),'), nl,
  write('trans(5,''|''/''|'',4),'), nl,
  write('trans(6,[]/'),write(V1),write(',5),'), nl,
  write('trans(6,$@(in(['),
    ltv1(ItemVarList,V1,LtV1),
    gen_comma_sep_list(LtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(LtV1),   
  write('])),6),'), nl,
  write('trans(6,'),write(V1),write('/'),write(V1),write(',5),'), nl,
  write('trans(7,# / #,6),'), nl,
  write('trans(7,'),write(V1),write('/[],7),'), nl,
  write('trans(7,$@(not_in([#,'),write(V1),write(']))/ $@(not_in([#,'),write(V1),write('])),7),'), nl,
  write('trans(8,'),write(C1),write('/'),write(C2),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=# and v1=v2.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_b(FOut,C1,C2,V12,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('9,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,1),'), nl,
  write('trans(3,/ / /,2),'), nl,
  write('trans(3,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),3),'), nl,
  write('trans(4,'),write(V12),write('/[],3),'), nl,
  write('trans(4,$@(not_in([]))/ $@(not_in([])),4),'), nl,
  write('trans(5,$@(in(['),
    gtv1(ItemVarList,V12,GtV12),
    gen_comma_sep_list(GtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV12),
    write('])),5),'), nl,
  write('trans(5,''|''/''|'',4),'), nl,
  write('trans(6,[]/'),write(V12),write(',5),'), nl,
  write('trans(6,$@(in(['),
    ltv1(ItemVarList,V12,LtV12),
    gen_comma_sep_list(LtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list(LtV12),   
  write('])),6),'), nl,
  write('trans(7,# / #,6),'), nl,
  write('trans(7,$@(not_in([#]))/ $@(not_in([#])),7),'), nl,
  write('trans(8,'),write(C1),write('/'),write(C2),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=m_f, mf precedes mt, and v1!=v2.
%-------------------------------------------------------------------------------

gen_branches_V1_assign_V2_next_c(_,[],_) :- !.

gen_branches_V1_assign_V2_next_c(V1,[Marker|Markers],I) :-
  ( (I=1, !, S4=4);
    (S4 is 8 + I) ),
  write('trans('),write(S4),write(','),write(Marker),write('_t /'),write(Marker),write('_t,3),'), nl,
  write('trans('),write(S4),write(','),write(V1),write('/[],'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),'),write(S4),write('),'), nl,
  write('trans(5,'),write(Marker),write('_f /'),write(Marker),write('_f,'),write(S4),write('),'), nl,
  II is I+1,
  gen_branches_V1_assign_V2_next_c(V1,Markers,II).

%...............................................................................

gen_V1_assign_V2_next_c(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + (Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  write('trans(2,/ / /,1),'), nl,
  write('trans(2,$@(in(['),
    gtv1(ItemVarList,V1,GtV1),
    gen_comma_sep_list(GtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV1),
    write('])),2),'), nl,
  write('trans(3,[]/'),write(V1),write(',2),'), nl,
  write('trans(3,$@(in(['),
    ltv1(ItemVarList,V1,LtV1),
    gen_comma_sep_list_mt(Markers),
    ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(Markers),
     ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),   
  write('])),3),'), nl,
  write('trans(3,'),write(V1),write('/'),write(V1),write(',2),'), nl,
  gen_branches_V1_assign_V2_next_c(V1,Markers,1),
  write('trans(6,/ / /,5),'), nl,
  write('trans(6,'),write(V1),write('/[],6),'), nl,
  write('trans(6,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),6),'), nl,
  write('trans(7,'),write(V1),write('/[],7),'), nl,
  write('trans(7,'),write(V2),write('/'),write(V2),write(',6),'), nl,
  write('trans(7,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),7),'), nl,
  write('trans(8,'),write(V1),write('/[],8),'), nl,
  write('trans(8,''|''/''|'',7),'), nl,
  write('trans(8,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),8),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=m_f, mt precedes mf, and v1!=v2.
%-------------------------------------------------------------------------------

gen_branches_V1_assign_V2_next_d(_,_,[],_,_,_) :- !.

gen_branches_V1_assign_V2_next_d(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6);
    (S2 is 8 + (I-2)*5 + 2,
     S3 is 8 + (I-2)*5 + 3,
     S4 is 8 + (I-2)*5 + 4,
     S5 is 8 + (I-2)*5 + 5,
     S6 is 8 + (I-2)*5 + 6) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(','),write(V1),write('/[],'),write(S3),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),
    write(',''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write('/[],'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(','),write(V2),write('/'),write(V2),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),'),
    write(S4),write('),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    gtv1(ItemVarList,V1,GtV1),
    gen_comma_sep_list(GtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV1),
    write('])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',[]/'),write(V1),write(','),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    ltv1(ItemVarList,V1,LtV1),
    gen_comma_sep_list_mt(AllMarkers),
    ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(AllMarkers),
     ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),   
  write('])),'),write(S6),write('),'), nl,
  write('trans('),write(S6),write(','),write(V1),write('/'),write(V1),write(','),write(S5),write('),'), nl,
  write('trans(7,'),write(Marker),write('_t/'),write(Marker),write('_t,'),write(S6),write('),'), nl,
  II is I+1,
  gen_branches_V1_assign_V2_next_d(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_assign_V2_next_d(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + (Len-1)*5,    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  gen_branches_V1_assign_V2_next_d(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(7,'),write(V1),write('/[],7),'), nl,
  write('trans(7,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),7),'), nl,
  write('trans(8,'),write(V1),write('/[],8),'), nl,
  write('trans(8,''|''/''|'',7),'), nl,
  write('trans(8,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),8),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=m_f, mf and mt yield a self-loop, v1!=v2, and 
% v1 precedes v2 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_V1_assign_V2_next_e(_,_,[],_,_,_) :- !.

gen_branches_V1_assign_V2_next_e(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5);
    (S2 is 7 + (I-2)*4 + 2,
     S3 is 7 + (I-2)*4 + 3,
     S4 is 7 + (I-2)*4 + 4,
     S5 is 7 + (I-2)*4 + 5) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(in(['),
    gtv1(ItemVarList,V2,GtV2),
    gen_comma_sep_list(GtV2),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV2),
    write('])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in(['),
    v1v2(ItemVarList,V1,V2,ItemVarListV1V2),
    gen_comma_sep_list(ItemVarListV1V2),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListV1V2),
    write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(','),write(V2),write('/'),write(V2),write(','),write(S3),write('),'), nl,
  write('trans('),write(S5),write(',[]/'),write(V1),write(','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    ltv1(ItemVarList,V1,LtV1),
    gen_comma_sep_list_mt(AllMarkers),
    ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(AllMarkers),
     ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),   
  write('])),'),write(S5),write('),'), nl,
  write('trans('),write(S5),write(','),write(V1),write('/'),write(V1),write(','),write(S4),write('),'), nl,
  write('trans(6,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_V1_assign_V2_next_e(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_assign_V2_next_e(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 9 + (Len-1)*4,
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  gen_branches_V1_assign_V2_next_e(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(6,'),write(V1),write('/[],6),'), nl,
  write('trans(6,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),6),'), nl,
  write('trans(7,'),write(V1),write('/[],7),'), nl,
  write('trans(7,''|''/''|'',6),'), nl,
  write('trans(7,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),7),'), nl,
  write('trans(8,'),write(C1),write('/'),write(C2),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=m_f, mf and mt yield a self-loop, v1!=v2, and 
% v2 precedes v1 in the ordered alphabet.
%-------------------------------------------------------------------------------

gen_branches_V1_assign_V2_next_f(_,_,[],_,_,_) :- !.

gen_branches_V1_assign_V2_next_f(V1,V2,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5);
    (S2 is 7 + (I-2)*4 + 2,
     S3 is 7 + (I-2)*4 + 3,
     S4 is 7 + (I-2)*4 + 4,
     S5 is 7 + (I-2)*4 + 5) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(in(['),
    gtv1(ItemVarList,V1,GtV1),
    gen_comma_sep_list(GtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV1),
    write('])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',[]/'),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write('/'),write(V1),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in(['),
    v1v2(ItemVarList,V2,V1,ItemVarListV2V1),
    gen_comma_sep_list(ItemVarListV2V1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(ItemVarListV2V1),
    write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    ltv1(ItemVarList,V2,LtV2),
    gen_comma_sep_list_mt(AllMarkers),
    ( (LtV2=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV2),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(AllMarkers),
     ( (LtV2=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV2),
  write('])),'),write(S5),write('),'), nl,
  write('trans('),write(S5),write(','),write(V2),write('/'),write(V2),write(','),write(S4),write('),'), nl,
  write('trans(6,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_V1_assign_V2_next_f(V1,V2,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_assign_V2_next_f(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 9 + (Len-1)*4,
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  gen_branches_V1_assign_V2_next_f(V1,V2,Markers,1,Markers,ItemVarList),
  write('trans(6,'),write(V1),write('/[],6),'), nl,
  write('trans(6,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),6),'), nl,
  write('trans(7,'),write(V1),write('/[],7),'), nl,
  write('trans(7,''|''/''|'',6),'), nl,
  write('trans(7,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),7),'), nl,
  write('trans(8,'),write(C1),write('/'),write(C2),write(',7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=m_f, mf precedes mt, and v1=v2.
%-------------------------------------------------------------------------------

gen_branches_V1_assign_V2_next_g([],_) :- !.

gen_branches_V1_assign_V2_next_g([Marker|Markers],I) :-
  ( (I=1, !, S4=4);
    (S4 is 8 + I) ),
  write('trans('),write(S4),write(','),write(Marker),write('_t /'),write(Marker),write('_t ,3),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans(5,'),write(Marker),write('_f /'),write(Marker),write('_f ,'),write(S4),write('),'), nl,
  II is I+1,
  gen_branches_V1_assign_V2_next_g(Markers,II).

%...............................................................................

gen_V1_assign_V2_next_g(FOut,C1,C2,V12,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + (Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,/ / /,1),'), nl,
  write('trans(2,$@(in(['),
    gtv1(ItemVarList,V12,GtV12),
    gen_comma_sep_list(GtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV12),
    write('])),2),'), nl,
  write('trans(3,[]/'),write(V12),write(',2),'), nl,
  write('trans(3,$@(in(['),
    ltv1(ItemVarList,V12,LtV12),
    gen_comma_sep_list_mt(Markers),
    ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(Markers),
     ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),   
  write('])),3),'), nl,
  gen_branches_V1_assign_V2_next_g(Markers,1),
  write('trans(6,/ / /,5),'), nl,
  write('trans(6,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),6),'), nl,
  write('trans(7,'),write(V12),write('/[],6),'), nl,
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,''|''/''|'',7),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=m_f, mt precedes mf, and v1=v2.
%-------------------------------------------------------------------------------

gen_branches_V1_assign_V2_next_h(_,[],_,_,_) :- !.

gen_branches_V1_assign_V2_next_h(V12,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6);
    (S2 is 8 + (I-2)*5 + 2,
     S3 is 8 + (I-2)*5 + 3,
     S4 is 8 + (I-2)*5 + 4,
     S5 is 8 + (I-2)*5 + 5,
     S6 is 8 + (I-2)*5 + 6) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(V12),write('/[],'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    gtv1(ItemVarList,V12,GtV12),
    gen_comma_sep_list(GtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV12),
    write('])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',[]/'),write(V12),write(','),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    ltv1(ItemVarList,V12,LtV12),
    gen_comma_sep_list_mt(AllMarkers),
    ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(AllMarkers),
     ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),   
  write('])),'),write(S6),write('),'), nl,
  write('trans(7,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),write(S6),write('),'), nl,
  II is I+1,
  gen_branches_V1_assign_V2_next_h(V12,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_assign_V2_next_h(FOut,C1,C2,V12,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 10 + (Len-1)*5,    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,9),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_V1_assign_V2_next_h(V12,Markers,1,Markers,ItemVarList),
  write('trans(7,$@(not_in([]))/ $@(not_in([])),7),'), nl,
  write('trans(8,''|''/''|'',7),'), nl,
  write('trans(8,$@(not_in([]))/ $@(not_in([])),8),'), nl,
  write('trans(9,'),write(C1),write('/'),write(C2),write(',8)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n=m_f, mf and mt yield a self-loop, and v1=v2.
%-------------------------------------------------------------------------------

gen_branches_V1_assign_V2_next_i(_,[],_,_,_) :- !.

gen_branches_V1_assign_V2_next_i(V12,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4);
    (S2 is 6 + (I-2)*3 + 2,
     S3 is 6 + (I-2)*3 + 3,
     S4 is 6 + (I-2)*3 + 4) ),
  write('trans('),write(S2),write(','),write(Marker),write('_f /'),write(Marker),write('_f ,1),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(in(['),
    gtv1(ItemVarList,V12,GtV12),
    gen_comma_sep_list(GtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV12),
    write('])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in(['),
    ltv1(ItemVarList,V12,LtV12),
    gen_comma_sep_list_mt(AllMarkers),
    ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(AllMarkers),
     ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),   
  write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(','),write(V12),write('/'),write(V12),write(','),write(S3),write('),'), nl,
  write('trans(5,'),write(Marker),write('_t /'),write(Marker),write('_t ,'),write(S4),write('),'), nl,
  II is I+1,
  gen_branches_V1_assign_V2_next_i(V12,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_V1_assign_V2_next_i(FOut,C1,C2,V12,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 8 + (Len-1)*3,
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_V1_assign_V2_next_i(V12,Markers,1,Markers,ItemVarList),
  write('trans(5,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(6,''|''/''|'',5),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,'),write(C1),write('/'),write(C2),write(',6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n != m_f nor #, and v1!=v2.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_j(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('8,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(1,'),write(V1),write('/[],1),'), nl,
  write('trans(1,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),1),'), nl,
  write('trans(2,/ / /,1),'), nl,
  write('trans(2,$@(in(['),
    gtv1(ItemVarList,V1,GtV1),
    gen_comma_sep_list(GtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV1),
    write('])),2),'), nl,
  write('trans(3,[]/'),write(V1),write(',2),'), nl,
  write('trans(3,$@(in(['),
    ltv1(ItemVarList,V1,LtV1),
    gen_comma_sep_list_mt(Markers),
    ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(Markers),
     ( (LtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV1),   
  write('])),3),'), nl,
  write('trans(3,'),write(V1),write('/'),write(V1),write(',2),'), nl,
  write('trans(4,/ / /,3),'), nl,
  write('trans(4,'),write(V1),write('/[],4),'), nl,
  write('trans(4,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),4),'), nl,
  write('trans(5,'),write(V1),write('/[],5),'), nl,
  write('trans(5,'),write(V2),write('/'),write(V2),write(',4),'), nl,
  write('trans(5,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),5),'), nl,
  write('trans(6,'),write(V1),write('/[],6),'), nl,
  write('trans(6,''|''/''|'',5),'), nl,
  write('trans(6,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),6),'), nl,
  write('trans(7,'),write(C1),write('/'),write(C2),write(',6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  write(''), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The situation of v2->n != m_f nor #, and v1=v2.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_k(FOut,C1,C2,V12,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('8,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,/ / /,1),'), nl,
  write('trans(2,$@(in(['),
    gtv1(ItemVarList,V12,GtV12),
    gen_comma_sep_list(GtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtV12),
    write('])),2),'), nl,
  write('trans(3,[]/'),write(V12),write(',2),'), nl,
  write('trans(3,$@(in(['),
    ltv1(ItemVarList,V12,LtV12),
    gen_comma_sep_list_mt(Markers),
    ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mt(Markers),
     ( (LtV12=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtV12),   
  write('])),3),'), nl,
  write('trans(4,/ / /,3),'), nl,
  write('trans(4,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),4),'), nl,
  write('trans(5,'),write(V12),write('/[],4),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(6,''|''/''|'',5),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,'),write(C1),write('/'),write(C2),write(',6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The summarized command for v1 < v2.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_for_V1_pred_V2(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  gen_V1_assign_V2_next_basic(':aux-0',C1,V2),
  gen_V1_assign_V2_next_a(':aux-1',C1,C2,V1,V2,ItemVarList), 
  gen_V1_assign_V2_next_c(':aux-2',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_assign_V2_next_d(':aux-3',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_assign_V2_next_e(':aux-4',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_assign_V2_next_j(':aux-5',C1,C2,V1,V2,Markers,ItemVarList),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3''),
    file('':aux-4''),file('':aux-5'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The summarized command for v2 < v1.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_for_V2_pred_V1(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  gen_V1_assign_V2_next_basic(':aux-0',C1,V2),
  gen_V1_assign_V2_next_a(':aux-1',C1,C2,V1,V2,ItemVarList), 
  gen_V1_assign_V2_next_c(':aux-2',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_assign_V2_next_d(':aux-3',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_assign_V2_next_f(':aux-4',C1,C2,V1,V2,Markers,ItemVarList),
  gen_V1_assign_V2_next_j(':aux-5',C1,C2,V1,V2,Markers,ItemVarList),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3''),
    file('':aux-4''),file('':aux-5'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The summarized command for v1 = v2.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next_for_V1_eq_v2(FOut,C1,C2,V12,Markers,ItemVarList) :-
  gen_V1_assign_V2_next_basic(':aux-0',C1,V12),
  gen_V1_assign_V2_next_b(':aux-1',C1,C2,V12,ItemVarList), 
  gen_V1_assign_V2_next_g(':aux-2',C1,C2,V12,Markers,ItemVarList),
  gen_V1_assign_V2_next_h(':aux-3',C1,C2,V12,Markers,ItemVarList),
  gen_V1_assign_V2_next_i(':aux-4',C1,C2,V12,Markers,ItemVarList),
  gen_V1_assign_V2_next_k(':aux-5',C1,C2,V12,Markers,ItemVarList),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1''),file('':aux-2''),file('':aux-3''),
    file('':aux-4''),file('':aux-5'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1 = v2->next; goto c2;
% The final command.
%-------------------------------------------------------------------------------

gen_V1_assign_V2_next(FOut,C1,C2,V,V,Markers,ItemVarList) :-
  gen_V1_assign_V2_next_for_V1_eq_v2(FOut,C1,C2,V,Markers,ItemVarList),
  !.
  
gen_V1_assign_V2_next(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  is_v1v2(ItemVarList,V1,V2),
  gen_V1_assign_V2_next_for_V1_pred_V2(FOut,C1,C2,V1,V2,Markers,ItemVarList),
  !.

gen_V1_assign_V2_next(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  gen_V1_assign_V2_next_for_V2_pred_V1(FOut,C1,C2,V1,V2,Markers,ItemVarList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: if (v1 == v2) goto c2; else goto c3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: v1 = v2; goto c2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_V1_assign_V2(FOut,C1,C2,V1,V2,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  ( (is_v1v2(ItemVarList,V1,V2), !, gen_V1_assign_V2_V1V2(C1,C2,V1,V2,Markers,ItemVarList));
    gen_V1_assign_V2_V2V1(C1,C2,V1,V2,Markers,ItemVarList) ),
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm),
  !.
  
%-------------------------------------------------------------------------------

gen_V1_assign_V2_V1V2(C1,C2,V1,V2,Markers,ItemVarList) :-
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
  write('trans(7,$@(in([#,'),
    ltv1(ItemVarList,V1,ItemVarListLtV1),
    gen_comma_sep_list_mt(Markers),
    ( (ItemVarListLtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(ItemVarListLtV1),
    write(']))/ $@(in([#,'),
    gen_comma_sep_list_mt(Markers),
    ( (ItemVarListLtV1=[],!);
      (write(',')) ),  
    gen_comma_sep_list(ItemVarListLtV1),
    write('])),7),'), nl,
  write('trans(7,'),write(V1),write('/'),write(V1),write(',6),'), nl,
  write('trans(8,'),write(V1),write('/[],8),'), nl,
  write('trans(8,$@(in([/,''|'']))/ $@(in([/,''|''])),7),'), nl,
  write('trans(8,$@(not_in(['),write(V1),write(']))/ $@(not_in(['),write(V1),write('])),8)'), nl,
  write('],'), nl,
  write('[]).'), nl.

%-------------------------------------------------------------------------------
  
gen_V1_assign_V2_V2V1(C1,C2,V1,V2,Markers,ItemVarList) :-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: if (v1 == 0) goto c2; else goto c3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: v1 = 0; goto c2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: if (v1->next == 0) goto c2; else goto c3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: v1->next = 0; goto c2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: v1->next = 0; goto c2;
% The basic situations (v1->next != mark_f).
%-------------------------------------------------------------------------------

gen_V1_next_assign_null_basic(FOut,C1,C2,V1,Markers) :-
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
  write('trans(2,'),write(V1),write(' /'),write(V1),write(' ,1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write(' /'),write(V1),write(' ,1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V1),write(' /'),write(V1),write(' ,1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write(' /'),write(C1),write(' ,2),'), nl,
  write('trans(7,'),write(C1),write(' /'),write(C2),write(' ,13),'), nl,
  write('trans(8,!/ #,1),'), nl,
  write('trans(8,# / #,1),'), nl,
  write('trans(8,[]/ #,9),'), nl,
  write('trans(9,[]/''|'',10),'), nl,
  write('trans(10,$@(not_in([!,#,'), nl,
    gen_all_from_markers(Markers),
    write(']))/ $@(not_in([!,#,'), nl,
    gen_all_from_markers(Markers),
    write('])),1),'), nl,
  write('trans(11,/ / /,8),'), nl,
  write('trans(11,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),11),'), nl,
  write('trans(12,'),write(V1),write(' /'),write(V1),write(' ,11),'), nl,
  write('trans(12,$@(not_in([]))/ $@(not_in([])),12),'), nl,
  write('trans(13,''|''/''|'',12),'), nl,
  write('trans(13,$@(not_in(['),write(V1),write(' ,''|'']))/ $@(not_in(['),write(V1),write(' ,''|''])),13)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = 0; goto c2;
% The case of v1->next = mark_f.
%-------------------------------------------------------------------------------

gen_branches_V1_next_assign_null_a(_,[],_,_,_) :- !.

gen_branches_V1_next_assign_null_a(V1,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6);
    (S1 is 8 + (I-2)*6 + 1,
     S2 is 8 + (I-2)*6 + 2,
     S3 is 8 + (I-2)*6 + 3,
     S4 is 8 + (I-2)*6 + 4,
     S5 is 8 + (I-2)*6 + 5,
     S6 is 8 + (I-2)*6 + 6) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t/[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),write(Marker),write('_t])),'),
    write(S1),write('),'), nl,
  write('trans('),write(S2),write(','),write(Marker),write('_f/ #,'),write(S1),write('),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(','),write(Marker),write('_t/[],'),write(S3),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,'),write(Marker),write('_t,''|'']))/ $@(not_in([/,'),
    write(Marker),write('_t,''|''])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(Marker),write('_t/[],'),write(S4),write('),'), nl,
  write('trans('),write(S4),write(','),write(V1),write(' /'),write(V1),write(' ,'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in(['),write(Marker),write('_t]))/ $@(not_in(['),write(Marker),write('_t])),'),
    write(S4),write('),'), nl,
  write('trans('),write(S5),write(',/ / /,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',[]/'),write(Marker),write('_f,'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mf(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(LtMarker),
    write('])),'),write(S6),write('),'), nl,
  write('trans(7,!/!,'),write(S6),write('),'), nl,
  II is I+1,
  gen_branches_V1_next_assign_null_a(V1,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_V1_next_assign_null_a(M,N) :-
  ( (M=1,!,F=1);
    (F is 9 + 6*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_V1_next_assign_null_a(MM,N)) ).

%...............................................................................

gen_V1_next_assign_null_a(FOut,C1,C2,V1,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 9 + 6*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_V1_next_assign_null_a(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,8),'), nl,
  gen_branches_V1_next_assign_null_a(V1,Markers,1,Markers,ItemVarList),
  write('trans(8,'),write(C1),write(' /'),write(C2),write(' ,7)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  write(''), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: v1->next = 0; goto c2;
% The summarized command.
%-------------------------------------------------------------------------------

gen_V1_next_assign_null(FOut,C1,C2,V1,Markers,ItemVarList) :-
  gen_V1_next_assign_null_basic(':aux-0',C1,C2,V1,Markers),
  gen_V1_next_assign_null_a(':aux-1',C1,C2,V1,Markers,ItemVarList),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: malloc(v1); goto c2; (if there is not enough memory, v1:=null)
% The next pointer of v1 is undefined, no data are associated with v1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: free(v1); goto c2;
% All aliases of v1 (i.e. pointer variables and markers) become undefined.
% All other associated information (labels, data) are deleted!!! (Unless they
% are claimed to be variables for the need of free...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: free(v1); goto c2;
% The basic situations.
%-------------------------------------------------------------------------------

gen_free_V1_basic(FOut,C1,C2,V1,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('24,'), nl,
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
  write('trans(2,'),write(V1),write(' /'),write(V1),write(' ,1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write(' /'),write(V1),write(' ,1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V1),write(' /'),write(V1),write(' ,1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write(' /'),write(C1),write(' ,2),'), nl,
  write('trans(7,'),write(C1),write(' /'),write(C2),write(' ,13),'), nl,
  write('trans(8,'),write(C1),write(' /'),write(C1),write(' ,23),'), nl,
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
  write('trans(19,$@(not_in(['), nl,
    gen_all_from_markers(Markers),
    write(']))/ $@(not_in(['), nl,
    gen_all_from_markers(Markers),
    write('])),1),'), nl,
  write('trans(20,/ / /,19),'), nl,
  write('trans(21,[]/''_f'',20),'), nl,
  write('trans(21,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),21),'), nl,
  write('trans(22,'),write(V1),write(' /'),write(V1),write(' ,21),'), nl,
  write('trans(22,$@(not_in([]))/ $@(not_in([])),22),'), nl,
  write('trans(23,''|''/''|'',22),'), nl,
  write('trans(23,$@(not_in([]))/ $@(not_in([])),23)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: free(v1); goto c2;
% Beginning of the moving aliases of the deleted element to unknown: the case of 
% the next pointer being a marker.
%-------------------------------------------------------------------------------

gen_branches_free_V1_a(_,[],_,_,_) :- !.

gen_branches_free_V1_a(V1,[Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5,S6=6,S7=7,S8=8);
    (S1 is 10 + (I-2)*8 + 1,
     S2 is 10 + (I-2)*8 + 2,
     S3 is 10 + (I-2)*8 + 3,
     S4 is 10 + (I-2)*8 + 4,
     S5 is 10 + (I-2)*8 + 5,
     S6 is 10 + (I-2)*8 + 6,
     S7 is 10 + (I-2)*8 + 7,
     S8 is 10 + (I-2)*8 + 8) ),
  write('trans('),write(S1),write(','),write(Marker),write('_t /[],'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_t ]))/ $@(not_in(['),write(Marker),write('_t ])),'),
    write(S1),write('),'), nl,
  write('trans('),write(S2),write(','),write(Marker),write('_f /!,'),write(S1),write('),'), nl,
  write('trans('),write(S3),write(',/ / /,'),write(S2),write('),'), nl,
  write('trans('),write(S4),write(',[]/''_f'','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(','),write(Marker),write('_t /[],'),write(S5),write('),'), nl,
  write('trans('),write(S5),write(','),write(V1),write(' /'),write(V1),write(' ,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in(['),write(Marker),write('_t ]))/ $@(not_in(['),
    write(Marker),write('_t ])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(','),write(Marker),write('_t /[],'),write(S6),write('),'), nl,
  write('trans('),write(S6),write(',''|''/''|'','),write(S5),write('),'), nl,
  write('trans('),write(S6),write(',$@(not_in(['),write(Marker),write('_t ]))/ $@(not_in(['),
    write(Marker),write('_t ])),'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',/ / /,'),write(S6),write('),'), nl,
  write('trans('),write(S7),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',[]/'),write(Marker),write('_f ,'),write(S7),write('),'), nl,
  write('trans('),write(S8),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mf(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(LtMarker),
    write('])),'),write(S8),write('),'), nl,
  write('trans(9,!/!,'),write(S8),write('),'), nl,
  II is I+1,
  gen_branches_free_V1_a(V1,Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_free_V1_a(M,N) :-
  ( (M=1,!,F=1);
    (F is 11 + 8*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_free_V1_a(MM,N)) ).

%...............................................................................

gen_free_V1_a(FOut,C1,V1,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 11 + 8*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_free_V1_a(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/d,10),'), nl,
  gen_branches_free_V1_a(V1,Markers,1,Markers,ItemVarList),
  write('trans(10,'),write(C1),write(' /'),write(C1),write(' ,9)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: free(v1); goto c2;
% Moving aliases of the deleted element to unknown: moving a variable.
%-------------------------------------------------------------------------------

gen_branches_free_move_a([],_,_,_) :- !.

gen_branches_free_move_a([Var|Variables],I,Markers,ItemVarList) :-
  ( (I=1, !, S3=3,S4=4,S5=5);
    (S3 is 5 + (I-2)*3 + 3,
     S4 is 5 + (I-2)*3 + 4,
     S5 is 5 + (I-2)*3 + 5) ),
  write('trans('),write(S3),write(','),write(Var),write(' /[],2),'), nl,
  write('trans('),write(S3),write(',$@(not_in([]))/ $@(not_in([])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',/ / /,'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in(['),
    gtv1(ItemVarList,Var,GtVar),
    gen_comma_sep_list(GtVar),
    write(']))/ $@(in(['),
    gen_comma_sep_list(GtVar),
    write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',[]/ '),write(Var),write(','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    ltv1(ItemVarList,Var,LtVar),
    gen_comma_sep_list_mf(Markers),
    ( (LtVar=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtVar),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(Markers),
     ( (LtVar=[],!);
      (write(',')) ),  
    gen_comma_sep_list(LtVar),   
  write('])),'),write(S5),write('),'), nl,
  write('trans(6,!/!,'),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_free_move_a(Variables,II,Markers,ItemVarList).

%...............................................................................

gen_free_move_a(FOut,Variables,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Variables,Len),
  N is 8 + (Len-1)*3,    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,d/d,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,''_f''/''_f'',1),'), nl,
  gen_branches_free_move_a(Variables,1,Markers,ItemVarList),
  write('trans(7,$@(not_in([]))/ $@(not_in([])),6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: free(v1); goto c2;
% Moving aliases of the deleted element to unknown: moving a marker.
%-------------------------------------------------------------------------------

gen_branches_free_move_b([],_,_,_) :- !.

gen_branches_free_move_b([Marker|Markers],I,AllMarkers,ItemVarList) :-
  ( (I=1, !, S1=1,S2=2,S3=3,S4=4,S5=5);
    (S1 is 7 + (I-2)*5 + 1,
     S2 is 7 + (I-2)*5 + 2,
     S3 is 7 + (I-2)*5 + 3,
     S4 is 7 + (I-2)*5 + 4,
     S5 is 7 + (I-2)*5 + 5) ),
  write('trans('),write(S1),write(','),write(Marker),write('_f /!,'),write(S1),write('),'), nl,
  write('trans('),write(S1),write(',$@(not_in(['),write(Marker),write('_f ]))/ $@(not_in(['),
    write(Marker),write('_f ])),'),write(S1),write('),'), nl,
  write('trans('),write(S2),write(',''_f''/''_f'','),write(S1),write('),'), nl,
  write('trans('),write(S3),write(','),write(Marker),write('_f /!,'),write(S3),write('),'), nl,
  write('trans('),write(S3),write(','),write(Marker),write('_t /[],'),write(S2),write('),'), nl,
  write('trans('),write(S3),write(',$@(not_in(['),write(Marker),write('_f ]))/ $@(not_in(['),
    write(Marker),write('_f ])),'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',/ / /,'),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(in(['),
    gtv1(AllMarkers,Marker,GtMarker),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(GtMarker),
    ( (GtMarker=[],!);
      (write(',')) ),
    gen_comma_sep_list(ItemVarList),
    write('])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',[]/'),write(Marker),write('_f ,'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(in(['),
    ltv1(AllMarkers,Marker,LtMarker),
    gen_comma_sep_list_mf(LtMarker),
    write(']))/ $@(in(['),
    gen_comma_sep_list_mf(LtMarker),
    write('])),'),write(S5),write('),'), nl,
  write('trans(6,!/!,'),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_free_move_b(Markers,II,AllMarkers,ItemVarList).

%...............................................................................

gen_final_free_move_b(M,N) :-
  ( (M=1,!,F=1);
    (F is 8 + 5*(M-2)) ),
  write(F),
  MM is M+1,
  ( (MM>N,!);
    (write(','),gen_final_free_move_b(MM,N)) ).

%...............................................................................

gen_free_move_b(FOut,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 8 + 5*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  gen_final_free_move_b(1,Len),nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,d/d,7),'), nl,
  gen_branches_free_move_b(Markers,1,Markers,ItemVarList),
  write('trans(7,$@(not_in([]))/ $@(not_in([])),6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: free(v1); goto c2;
% Moving aliases of the deleted element to unknown: removing an item that's not
% a variable nor a marker...
%-------------------------------------------------------------------------------

gen_free_move_c(FOut,Variables,Markers) :-
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
  write('trans(3,$@(not_in([]))/ $@(not_in([])),3),'), nl,
  write('trans(3,not_in([/,'),
    gen_all_to_markers(Markers),
    write(','),
    gen_comma_sep_list(Variables),
    write(',''|''])/[],2),'), nl,
  write('trans(4,!/!,3),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),4)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: free(v1); goto c2;
% The summarized command.
%-------------------------------------------------------------------------------

%% May be optimized by separating the generation of the basis and the moving,
%% which is always the same - here, it is as follows in order to make sure
%% the moving is never omitted.

gen_free_list_elm_V1(FOut1,FOut2,C1,C2,V1,Variables,Markers,ItemVarList) :-
  gen_free_V1_basic(':aux-0',C1,C2,V1,Markers),
  gen_free_V1_a(':aux-1',C1,V1,Markers,ItemVarList),
  fsa_regex_atom_compile('{file('':aux-0''),file('':aux-1'')}',ResAtm1),
  fsa_write_file(FOut1,ResAtm1),
  gen_free_move_a(':aux-2',Variables,Markers,ItemVarList),
  gen_free_move_b(':aux-3',Markers,ItemVarList),
  gen_free_move_c(':aux-4',Variables,Markers),
  fsa_regex_atom_compile('{file('':aux-2''),file('':aux-3''),file('':aux-4'')}',ResAtm2),
  fsa_write_file(FOut2,ResAtm2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Normalization: we try to remove as many m_f/m_t marker pairs as possible.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% We are normalizing, we can shift, and so we start shifting - the situation of 
% mark_f followed by mark_t.
%-------------------------------------------------------------------------------

gen_norm_start_shifting_a(FOut,C1,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 13 + 9*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/s,12),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  gen_branches_start_shifting_a(Markers,1,Markers,ItemVarList),
  write('trans(12,'),write(C1),write(' / '),write(C1),write(',11)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% We are normalizing, we can shift, and so we start shifting - the situation of 
% mark_t followed by mark_f.
%-------------------------------------------------------------------------------

gen_norm_start_shifting_b(FOut,C1,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 14 + 9*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/s,13),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,''|''/[],1),'), nl,
  gen_branches_start_shifting_b(Markers,1,Markers,ItemVarList),
  write('trans(13,'),write(C1),write(' / '),write(C1),write(',12)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% We are normalizing, but we cannot shift any more (no marker can be released), 
% and so we are done.
%-------------------------------------------------------------------------------

%%% Created by adding a state to gen_to_unknown(FOut,Markers)...

gen_norm_end(FOut,C1,C2,Markers) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Markers,Len),
  N is 6 + 2*(Len-1),    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  AddedState is 5 + 2*(Len-1),  
  write('trans(0,n/n,'),write(AddedState),write('),'), nl,
  write('trans('),write(AddedState),write(','),write(C1),write(' / '),write(C2),write(',2),'), nl,
  write('trans(1,/ / /,2),'), nl,
  gen_branches_to_unknown(Markers,1),
  write('trans(1,$@(not_in([/,'), nl,
    gen_all_to_markers(Markers),
    write(',''|'']))/ $@(not_in([/,'), nl,
    gen_all_to_markers(Markers),
    write(',''|''])),1),'), nl,
  write('trans(2,''|''/''|'',1),'), nl,
  write('trans(2,$@(not_in([''|'']))/ $@(not_in([''|''])),2)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% The summarized command for normalization.
%-------------------------------------------------------------------------------

%% May be optimized by separating the generation of the basis and the shifting,
%% which is always the same - here, it is as follows in order to make sure
%% the shifting is never omitted.

gen_normalization(FOut1,FOut2,C1,C2,Markers,ItemVarList) :-
  gen_norm_start_shifting_a(':aux-1',C1,Markers,ItemVarList),
  gen_norm_start_shifting_b(':aux-2',C1,Markers,ItemVarList),
  gen_norm_end(':aux-3',C1,C2,Markers),
  fsa_regex_atom_compile('{file('':aux-1''),file('':aux-2''),file('':aux-3'')}',ResAtm),
  fsa_write_file(FOut1,ResAtm),
  gen_V1_next_assign_V2_shifting(FOut2,Markers,ItemVarList).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: if (*v1 == const); goto c2; else goto c3;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: if (*v1 == const); goto c2; else goto c3;
% The cases of errors.
%-------------------------------------------------------------------------------

gen_if_starV1_eq_const_errors(FOut,C1,V1) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('7,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,6),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,3),'), nl,
  write('trans(2,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write(' / '),write(C1),write(',2)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (*v1 == const); goto c2; else goto c3;
% The condition holds.
%-------------------------------------------------------------------------------

gen_if_starV1_eq_const_true(FOut,C1,C2,V1,A) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('7,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,6),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(2,$@(not_in([/,'),write(V1),write(',''|'']))/ $@(not_in([/,'),write(V1),write(',''|''])),2),'), nl,
  write('trans(3,'),write(A),write(' /'),write(A),write(' ,1),'), nl,
  write('trans(3,$@(not_in([/,'),write(A),write(' ,''|'']))/ $@(not_in([/,'),write(A),write(' ,''|''])),3),'), nl,
  write('trans(4,'),write(A),write(' /'),write(A),write(' ,2),'), nl,
  write('trans(4,'),write(V1),write(' / '),write(V1),write(',3),'), nl,
  write('trans(4,$@(not_in([]))/ $@(not_in([])),4),'), nl,
  write('trans(5,''|''/''|'',4),'), nl,
  write('trans(5,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(6,'),write(C1),write(' / '),write(C2),write(',5)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  write(''), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (*v1 == const); goto c2; else goto c3;
% The condition does not hold.
%-------------------------------------------------------------------------------

gen_if_starV1_eq_const_false(FOut,C1,C3,V1,A) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('8,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,/ / /,1),'), nl,
  write('trans(2,$@(not_in([/,'),write(A),write(' ,''|'']))/ $@(not_in([/,'),write(A),write(' ,''|''])),2),'), nl,
  write('trans(3,'),write(V1),write(' /'),write(V1),write(' ,2),'), nl,
  write('trans(3,$@(in([/,''|'']))/ $@(in([/,''|''])),3),'), nl,
  write('trans(3,$@(not_in([/,'),write(A),write(' ,''|'']))/ $@(not_in([/,'),write(A),write(' ,''|''])),4),'), nl,
  write('trans(3,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(4,'),write(V1),write(' /'),write(V1),write(' ,2),'), nl,
  write('trans(4,$@(not_in([/,'),write(A),write(' ,''|'']))/ $@(not_in([/,'),write(A),write(' ,''|''])),4),'), nl,
  write('trans(5,$@(in([/,''|'']))/ $@(in([/,''|''])),3),'), nl,
  write('trans(5,$@(not_in([/,''|'']))/ $@(not_in([/,''|''])),5),'), nl,
  write('trans(6,''|''/''|'',3),'), nl,
  write('trans(6,$@(not_in([]))/ $@(not_in([])),6),'), nl,
  write('trans(7,'),write(C1),write(' /'),write(C3),write(' ,6)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: if (*v1 == const); goto c2; else goto c3;
% The summarized command.
%-------------------------------------------------------------------------------

gen_if_starV1_eq_const(FOut,C1,C2,C3,V1,A) :-
  gen_if_starV1_eq_const_errors(':aux-1',C1,V1),
  gen_if_starV1_eq_const_true(':aux-2',C1,C2,V1,A),
  gen_if_starV1_eq_const_false(':aux-3',C1,C3,V1,A),
  fsa_regex_atom_compile('{file('':aux-1''),file('':aux-2''),file('':aux-3'')}',ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: *v1 = const; goto c2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: *v1 = const; goto c2;
% The second phase: writing the new value of *v1.
% (The first phase - erasing the old value - is the second phase of *v1 = *v2.)
%-------------------------------------------------------------------------------

gen_do_starV1_assign_const(FOut,C1,C2,V1,A,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('8,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/n,7),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  ( (is_v1v2(ItemVarList,V1,A), !,
     write('trans(2,/ / /,1),'), nl,
     write('trans(2,$@(in(['),
       gtv1(ItemVarList,A,GtA),
       gen_comma_sep_list(GtA),
       write(']))/ $@(in(['),
       gen_comma_sep_list(GtA),
       write('])),2),'), nl,
     write('trans(4,[]/'),write(A),write(',2),'), nl,
     write('trans(4,$@(in(['),
       v1v2(ItemVarList,V1,A,V1A),
       gen_comma_sep_list(V1A),
       write(']))/ $@(in(['),
       gen_comma_sep_list(V1A),
       write('])),4),'), nl,
     write('trans(5,'),write(V1),write(' / '),write(V1),write(',4),'), nl);
    (write('trans(5,$@(in([/,''|'']))/ $@(in([/,''|''])),6),'), nl,
     write('trans(6,[]/'),write(A),write(' ,3),'), nl,
     write('trans(6,$@(in(['),
       ltv1(ItemVarList,A,LtA),
       gen_all_to_markers(Markers),
       ( (LtA=[],!);
         (write(',')) ),
       gen_comma_sep_list(LtA),
       write(']))/ $@(in(['),
       gen_all_to_markers(Markers),
       ( (LtA=[],!);
         (write(',')) ),
       gen_comma_sep_list(LtA),
       write('])),6),'), nl,
     write('trans(3,$@(in(['),
       v1v2(ItemVarList,A,V1,AV1),
       gen_comma_sep_list(AV1),
       write(']))/ $@(in(['),
       gen_comma_sep_list(AV1),
       write('])),3),'), nl,
     write('trans(3,'),write(V1),write(' / '),write(V1),write(',1),'), nl) ),
  write('trans(5,$@(not_in([]))/ $@(not_in([])),5),'), nl,
  write('trans(7,'),write(C1),write('/'),write(C2),write(',5)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: *v1 = const; goto c2;
% The summarized command.
%-------------------------------------------------------------------------------

gen_starV1_assign_const(FOut,C1,C2,V1,A,Values,Markers,ItemVarList) :-
  gen_if_starV1_eq_const_errors(':aux-0',C1,V1),
  gen_starV1_assign_starV2_phase2(':aux-1',V1,Values),
  gen_do_starV1_assign_const(':aux-2',C1,C2,V1,A,Markers,ItemVarList),
  fsa_regex_atom_compile('{ file('':aux-0''),(cleanup(file('':aux-1'')) o cleanup(file('':aux-2''))) }',ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: if (*v1 == *v2); goto c2; else goto c3;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% To be added...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1: *v1 = *v2; goto c2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------
% c1: *v1 = *v2; goto c2;
% The cases of errors.
%-------------------------------------------------------------------------------

gen_starV1_assign_starV2_errors(FOut,C1,C2,V1,V2) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('7,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,n/e,6),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,# / #,3),'), nl,
  write('trans(2,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(2,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(2,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(2,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(3,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(3,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(3,$@(not_in([''|'']))/ $@(not_in([''|''])),3),'), nl,
  write('trans(4,# / #,3),'), nl,
  write('trans(4,$@(not_in([#]))/ $@(not_in([#])),4),'), nl,
  write('trans(5,'),write(V1),write(' / '),write(V1),write(',1),'), nl,
  write('trans(5,'),write(V2),write(' / '),write(V2),write(',1),'), nl,
  write('trans(5,$@(not_in([/]))/ $@(not_in([/])),5),'), nl,
  write('trans(6,'),write(C1),write(' / '),write(C1),write(',2)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: *v1 = *v2; goto c2;
% The first phase: reading the value of *v2.
%-------------------------------------------------------------------------------

gen_branches_starV1_assign_starV2_phase1(_,_,[],_) :- !.

gen_branches_starV1_assign_starV2_phase1(C1,V2,[A|Values],I) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6);
    (S2 is 5 + (I-2)*5 + 2,
     S3 is 5 + (I-2)*5 + 3,
     S4 is 5 + (I-2)*5 + 4,
     S5 is 5 + (I-2)*5 + 5,
     S6 is 5 + (I-2)*5 + 6) ),
  write('trans(0,n/'),write(A),write(','),write(S6),write('),'), nl,
  write('trans('),write(S2),write(','),write(V2),write('/'),write(V2),write(',1),'), nl,
  write('trans('),write(S2),write(',$@(not_in([/,'),write(V2),write(',''|'']))/ $@(not_in([/,'),write(V2),write(',''|''])),'),
    write(S2),write('),'), nl,
  write('trans('),write(S3),write(','),write(A),write(' / '),write(A),write(',1),'), nl,
  write('trans('),write(S3),write(',$@(not_in([/,'),write(A),write(',''|'']))/ $@(not_in([/,'),write(A),write(',''|''])),'),
    write(S3),write('),'), nl,
  write('trans('),write(S4),write(','),write(A),write(' / '),write(A),write(','),write(S2),write('),'), nl,
  write('trans('),write(S4),write(','),write(V2),write('/'),write(V2),write(','),write(S3),write('),'), nl,
  write('trans('),write(S4),write(',$@(not_in([]))/ $@(not_in([])),'),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',''|''/''|'','),write(S4),write('),'), nl,
  write('trans('),write(S5),write(',$@(not_in([]))/ $@(not_in([])),'),write(S5),write('),'), nl,
  write('trans('),write(S6),write(','),write(C1),write('/'),write(C1),write(','),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_starV1_assign_starV2_phase1(C1,V2,Values,II).

%...............................................................................

gen_starV1_assign_starV2_phase1(FOut,C1,V2,Values) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Values,Len),
  N is 7 + (Len-1)*5,    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  gen_branches_starV1_assign_starV2_phase1(C1,V2,Values,1),
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: *v1 = *v2; goto c2;
% The second phase: erasing the current value of *v1.
%-------------------------------------------------------------------------------

gen_starV1_assign_starV2_phase2(FOut,V1,Values) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  write('4,'), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  write('trans(0,$@(in([/,''|'']))/ $@(in([/,''|''])),3),'), nl,
  write('trans(0,$@(not_in([]))/ $@(not_in([])),0),'), nl,
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1),'), nl,
  write('trans(2,/ / /,1),'), nl,
  write('trans(2,in(['),
    gen_comma_sep_list(Values),
    write('])/[],2),'), nl,
  write('trans(2,$@(not_in([/,'),
    gen_comma_sep_list(Values),
    write(',''|'']))/ $@(not_in([/,'),
    gen_comma_sep_list(Values),
    write(',''|''])),2),'), nl,
  write('trans(3,'),write(V1),write('/'),write(V1),write(',2),'), nl,
  write('trans(3,in(['),
    gen_comma_sep_list(Values),
    write('])/[],3),'), nl,
  write('trans(3,$@(not_in([/,'),
    gen_comma_sep_list(Values),
    write(',''|'']))/ $@(not_in([/,'),
    gen_comma_sep_list(Values),
    write(',''|''])),3)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: *v1 = *v2; goto c2;
% The third phase: writing the new value of *v1.
%-------------------------------------------------------------------------------

gen_branches_starV1_assign_starV2_phase3(_,_,_,[],_,_,_) :- !.

gen_branches_starV1_assign_starV2_phase3(C1,C2,V1,[A|Values],I,Markers,ItemVarList) :-
  ( (I=1, !, S2=2,S3=3,S4=4,S5=5,S6=6,S7=7);
    (S2 is 6 + (I-2)*6 + 2,
     S3 is 6 + (I-2)*6 + 3,
     S4 is 6 + (I-2)*6 + 4,
     S5 is 6 + (I-2)*6 + 5,
     S6 is 6 + (I-2)*6 + 6,
     S7 is 6 + (I-2)*6 + 7) ),
  write('trans(0,'),write(A),write(' /n,'),write(S7),write('),'), nl,
  ( (is_v1v2(ItemVarList,V1,A), !,
     write('trans('),write(S2),write(',/ / /,1),'), nl,
     write('trans('),write(S2),write(',$@(in(['),
       gtv1(ItemVarList,A,GtA),
       gen_comma_sep_list(GtA),
       write(']))/ $@(in(['),
       gen_comma_sep_list(GtA),
       write('])),'),write(S2),write('),'), nl,
     write('trans('),write(S4),write(',[]/'),write(A),write(','),write(S2),write('),'), nl,
     write('trans('),write(S4),write(',$@(in(['),
       v1v2(ItemVarList,V1,A,V1A),
       gen_comma_sep_list(V1A),
       write(']))/ $@(in(['),
       gen_comma_sep_list(V1A),
       write('])),'),write(S4),write('),'), nl,
     write('trans('),write(S5),write(','),write(V1),write(' / '),write(V1),write(','),write(S4),write('),'), nl);
    (write('trans('),write(S5),write(',$@(in([/,''|'']))/ $@(in([/,''|''])),'),write(S6),write('),'), nl,
     write('trans('),write(S6),write(',[]/'),write(A),write(' ,'),write(S3),write('),'), nl,
     write('trans('),write(S6),write(',$@(in(['),
       ltv1(ItemVarList,A,LtA),
       gen_all_to_markers(Markers),
       ( (LtA=[],!);
         (write(',')) ),
       gen_comma_sep_list(LtA),
       write(']))/ $@(in(['),
       gen_all_to_markers(Markers),
       ( (LtA=[],!);
         (write(',')) ),
       gen_comma_sep_list(LtA),
       write('])),'),write(S6),write('),'), nl,
     write('trans('),write(S3),write(',$@(in(['),
       v1v2(ItemVarList,A,V1,AV1),
       gen_comma_sep_list(AV1),
       write(']))/ $@(in(['),
       gen_comma_sep_list(AV1),
       write('])),'),write(S3),write('),'), nl,
     write('trans('),write(S3),write(','),write(V1),write(' / '),write(V1),write(',1),'), nl) ),
  write('trans('),write(S5),write(',$@(not_in([]))/ $@(not_in([])),'),write(S5),write('),'), nl,
  write('trans('),write(S7),write(','),write(C1),write('/'),write(C2),write(','),write(S5),write('),'), nl,
  II is I+1,
  gen_branches_starV1_assign_starV2_phase3(C1,C2,V1,Values,II,Markers,ItemVarList).

%...............................................................................

gen_starV1_assign_starV2_phase3(FOut,C1,C2,V1,Values,Markers,ItemVarList) :-
  open(':aux',write,SOut),
  tell(SOut),
  write('fa('), nl,
  write('t(fsa_preds,fsa_preds),'), nl,
  length(Values,Len),
  N is 8 + (Len-1)*6,    
  write(N),write(','), nl,
  write('['), nl,
  write('0'), nl,
  write('],'), nl,
  write('['), nl,
  write('1'), nl,
  write('],'), nl,
  write('['), nl,
  gen_branches_starV1_assign_starV2_phase3(C1,C2,V1,Values,1,Markers,ItemVarList),
  write('trans(1,$@(not_in([]))/ $@(not_in([])),1)'), nl,
  write('],'), nl,
  write('[]).'), nl,
  told,
  fsa_regex_compile(file(':aux'),AuxAtm),
  sort_trans(AuxAtm,ResAtm),
  fsa_write_file(FOut,ResAtm).

%-------------------------------------------------------------------------------
% c1: *v1 = *v2; goto c2;
% The summarized command.
%-------------------------------------------------------------------------------

gen_starV1_assign_starV2(FOut,C1,C2,V1,V2,Values,Markers,ItemVarList) :-
  gen_starV1_assign_starV2_errors(':aux-0',C1,C2,V1,V2),
  gen_starV1_assign_starV2_phase1(':aux-1',C1,V2,Values),
  gen_starV1_assign_starV2_phase2(':aux-2',V1,Values),
  gen_starV1_assign_starV2_phase3(':aux-3',C1,C2,V1,Values,Markers,ItemVarList),
  fsa_regex_atom_compile('{ file('':aux-0''),
    (cleanup(file('':aux-1'')) o cleanup(file('':aux-2'')) o cleanup(file('':aux-3''))) }',ResAtm),
  fsa_write_file(FOut,ResAtm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorting items on the arcs of generated transducers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


