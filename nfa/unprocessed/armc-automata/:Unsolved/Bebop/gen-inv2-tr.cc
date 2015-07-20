#include <iostream>
#include <iomanip>

#include <stdlib.h>

int main(int argc,char *argv[]) {

  int n;

  if (argc!=2) {
    cerr << "One numerical argument is to be provided!\n";
    return 1;
  }
  
  n=atoi(argv[1]);

  cout << "fa(\n\n";

  cout << "%begin sigma and symbols\n";
  cout << "t(fsa_preds,fsa_preds),\n";
  cout << "%end sigma and symbols\n\n";

  cout << (n-1)*6+3+5+1 << ", % number of states\n\n";

  cout << "[ % begin start states\n";
  cout << "0\n";
  cout << "], % end start states\n\n";

  cout << "[ % begin final states\n";
  cout << "1\n";
  cout << "], % end final states\n\n";

  cout << "[ % begin transitions\n\n";

  for (int i=n;i>0;i--) {

    cout << "%%%%%%% LEVEL " << i << "\n\n";

    cout << "trans(0,$@(in(['0','1']))/ $@(in(['0','1']))," << (n-i)*6+3 << "),\n";
    
    cout << "trans(" << (n-i)*6+3 << ",$@(in([a,b,c,d,e,f]))/ $@(in([a,b,c,d,e,f]))," << (n-i)*6+3+1 << "),\n";
    
    cout << "trans(" << (n-i)*6+3+1 << ",'" << i << "'/'" << i << "'," << (n-i)*6+3+2 << "),\n";

    cout << "trans(" << (n-i)*6+3+2 << ",$@(in(['0','1']))/ $@(in(['0','1']))," << (n-i)*6+3+3 << "),\n";

    cout << "trans(" << (n-i)*6+3+3 << ",$@(in(['0','1']))/ $@(in(['0','1']))," << (n-i)*6+3+4 << "),\n";

    cout << "trans(" << (n-i)*6+3+4 << ",$@(in(['0','1']))/ $@(in(['0','1']))," << (n-i)*6+3+5 << "),\n";

    cout << "trans(" << (n-i)*6+3+5 << ",$@(in([d,e]))/ $@(in([d,e]))," << (n-i)*6+3+7 << "),\n\n";
    
  }
  
  cout << "%%%%%%%  THE MAIN FUNCTION LEVEL\n\n";
  
  cout << "trans(3,$@(in([p,q,r,s,t]))/ $@(in([p,q,r,s,t])),2),\n";
  cout << "trans(" << (n-1)*6+3+5 << ",$@(in([q,r]))/ $@(in([q,r])),2),\n";
  cout << "trans(2,$@(in(['0','1']))/ $@(in(['0','1'])),1)\n\n";

  cout << "], % end transitions\n";
  cout << "[]). % jumps\n";
  
  return 0;
}
