%% Automatically generated by FSA Utilities.
%% For more info, cf. http://www.let.rug.nl/~vannoord/Fsa/

fa(
%begin sigma and symbols
t(fsa_preds,fsa_preds),
%end sigma and symbols
13, % number of states
[ % begin start states
0
], % end start states
[ % begin final states
4
], % end final states
[ % begin transitions

trans(0,l1/cln,1),
trans(0,l2/cln,1),


trans(1,qs2/qs0,2),


trans(2,qs0/qs0,5),
trans(2,qs1/qs1,6),
trans(2,qs2/qs2,7),
trans(2,qs3/qs3,8),


trans(5,[]/qs3,9),

trans(6,[]/qs3,10),

trans(7,[]/qs3,11),

trans(8,[]/qs3,12),


trans(9,[]/qs0,3),

trans(10,[]/qs1,3),

trans(11,[]/qs2,3),

trans(12,[]/qs3,3),


trans(3,[]/m,4),


trans(4,$@(not_in([]))/ $@(not_in([])),4)


], % end transitions
[]). % jumps















