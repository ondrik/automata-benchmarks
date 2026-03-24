# TELA-benchmarks
## Existing benchmarks
Filtering to obtain non-trivial automata was done as follows: 
1. remove automata with empty language,
2. remove deterministic automata,
3. remove inherently weak automata.

formulae are from https://github.com/jurajmajor/ltl3tela/tree/master/Experiments/formulae repository.

Folder `elevatorized` contains elevatorized automata from `filtered` folder, using `kofola` (elevatorization of nonbuchi NACs).

## Randomly generated benchmarks
These benchmarks were generated using:
```
	randltl -L --seed=42128971 --weak-fairness -n -1 --simplify=0 a b c d e
```
The generated LTL formulas were then translated to HOA automata using:
```
	ltl3tela -o hoa -f formula
```
We applied additional filtering to obtain non-trivial and relevant instances:
- `autfilt` was used to exclude trivial automata (e.g., deterministic, weak, empty, or terminal ones).
- `kofola` was used to retain only ELEVATOR automata.
