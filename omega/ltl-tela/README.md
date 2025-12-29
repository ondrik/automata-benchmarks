# TELA-benchmarks
Filtering to obtain non-trivial automata was done as follows: 
1. remove automata with empty language,
2. remove deterministic automata,
3. remove inherently weak automata.

formulae are from https://github.com/jurajmajor/ltl3tela/tree/master/Experiments/formulae repository.

Folder `elevatorized` contains elevatorized automata from `filtered` folder, using `kofola` (elevatorization of nonbuchi NACs).
