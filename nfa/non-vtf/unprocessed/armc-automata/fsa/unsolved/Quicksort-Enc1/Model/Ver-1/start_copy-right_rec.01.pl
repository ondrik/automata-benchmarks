%% Automatically generated by FSA Utilities.
%% For more info, cf. http://www.let.rug.nl/~vannoord/Fsa/

fa(
%begin sigma and symbols
t(fsa_preds,fsa_preds),
%end sigma and symbols
5, % number of states
[ % begin start states
0
], % end start states
[ % begin final states
4
], % end final states
[ % begin transitions

trans(0,b/c,1),


trans(1,qs2/qs0,2),


trans(2,[]/qs3,3),


trans(3,[]/m,4),


trans(4,$@(not_in([]))/ $@(not_in([])),4)


], % end transitions
[]). % jumps















