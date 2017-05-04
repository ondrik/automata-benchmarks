# The .vtf format for automata
This is a proposal for the `.vtf` (VATA Format) format for automata.

## Examples


### Finite automata
[link](nfa-example.vtf)
```
# Example of the VATA format for storing or exchanging automata
#
# comments start with '#'

@NFA       # denotes the type of the automaton that will follow
           # (nondeterministic finite automaton); the @type preamble starts a
           # section that will (in this case) define one automaton; the section
           # ends either with an end-of-file, or with another @type preamble

# now, we follow with the definition of components of an automaton
%Name nfa1                        # name of the automaton (optional, can be used to refer to the automaton)
 %Alphabet a b c d                # alphabet (optional) (a whitespace before % is OK)
%Initial                          # initial states (required)
  q1                              # a definition spans until another %definition
  "a state"                       # when in ", names can have whitespaces (and also " if escaped with backslash '\')
  "\"we're here,\" he said"       # a state with the name |"we're here," he said| ('|' are not part of the name)
                                  # names cannot span multiple lines
  q3                              # initial states end here
%Final q2                         # final states (required)
%Transitions                      # starting below this line will be transitions, one per line
q1 a q2                           # the format is <source> <symbol> <target> 
"q1" b "a state"                  # note that "q1" and q1 are the same
"\"we're here,\" he said" c q1

```
### Tree automata
[link](nta-example.vtf)
```
# Example of tree automata in the VATA format
@NTA               # nondeterministic tree automaton
%Root q2           # root states (required)
%Transitions
q1 a (q1 q2)       # the format is <parent> <symbol> (<child_1> ... <child_n>)
"q1" b "q1"        # is equivalent to q1 b (q1)
q2 c               # is equivalent to q2 c ()

```
### Finite automata with transitions in BDDs
[link](nfa-bdd-example.vtf)
```
# Example of finite automata with transitions in a BDD in the VATA format
@NFA-BDD          # NFAs with transitions in BDD
%Symbol-Vars 8    # number of Boolean variables in the alphabet (required)
%Initial q1 q2
%Final q2
%Transitions
q1 000x11x1 q2    # the format is <source> <symbol> <target> 
q1 01101111 q3    # 'x' in the binary vector denote don't care values
q3 xxxxxxxx q1    # the length needs to match the value in '%Symbol-Vars'

```
### Finite automata with everything in BDDs
[link](nfa-bdd-full-example.vtf)
```
# Example of finite automata where both states and transitions are in a BDD in the VATA format
@NFA-BDD-FULL     # NFAs with states and transitions in BDD
%State-Vars 3     # number of Boolean variables in states (required)
%Symbol-Vars 8    # number of Boolean variables in the alphabet (required)
%Initial 111 1x1
%Final 00x
%Transitions
111 000x11x1 0x0  # the format is <source> <symbol> <target> 
xxx xx11xx00 11x  # 'x' in the binary vectors denote don't care values

```
### A sequence of operations
[link](code.vtf)
```
# Example of how to define a sequence of operations in the VATA format

@NFA
%Name nfa1
%Initial q1
%Final q2
%Transitions
q1 a q2

@NFA
%Name nfa2
%Initial r1
%Final r2
%Transitions
r1 a r2

@CODE                  # some code comes here
NFA nfa3 = (minus (union nfa1 nfa2) (intersect nfa1 nfa2))
bool empty = (isempty nfa3)
(print "NFA3:\n")
(print NFA3)
(print "is empty:")
(print empty)
(return empty)

```

### Symbolic finite automaton
[link](sfa-example.vtf)
```
# Example of a symbolic finite automata (in the sense of Margus & Loris) in the VATA format [TENTATIVE PROPOSAL, NOT FIXED!!!]
@SFA               # symbolic finite automaton
%Name sfa1         # identifier (optional)
%Initial q1        # initial states (required)
%Final q2          # root states (required)
# TODO: maybe specify theories?
%Transitions
q1 "(even x)" q1   # the format is <parent> <formula> (<child_1> ... <child_n>)
"q1" "(odd x)" q1  # 'x' in the formula denotes the read symbol
q2 "(= x 3)" q3

```
