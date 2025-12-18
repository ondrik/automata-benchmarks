# TELA-benchmarks
Benchmarks obtained by:
```
    ./gen.sh formulae_files ltl3tela     // creates outputs_TIMESTAMP dir with automata and classification file (.csv) - tool ltl3tela was used for LTL -> TELA
    ./filter.sh classification_file      // takes classification of automata and produces filtered.csv (classification) and creates folder 'filtered' with respective .hoa files
```
Filtering to obtain non-trivial automata was done as follows: 
1. remove automata with empty language,
2. remove deterministic automata,
3. remove inherently weak automata.

`formulae/` is from https://github.com/jurajmajor/ltl3tela/tree/master/Experiments/formulae repository.
