# Proposal for VTF `@CODE` semantics
The basic syntax of `@CODE` sections is according to the [basic .vtf syntax](README.md), i.e., the section begins with a line with `@CODE` and a sequence of lines until the next `@`-header or end of file follows.
The lines can start with `%`, in which case they are *meta* lines, or not, in which case they are body lines.
The format is line-oriented.
This document describes the syntax and semantics of a `@CODE` section in more detail.

## Body
The body of a `@CODE` section is a sequence of statements, separated by ends of lines.
Programs are straight-line, i.e., no control flow or loops statements are present (at least for now).
Statements are either **variable assignments** or **procedure calls**.
**Variable assignments** are of the form `lhs = expr`, e.g.,
```
aut = (load_aut "nfa1.vtf")
```
where `aut` is a variable name and `(load_aut "nfa1.vtf")` is the expression the value of which is to be assigned to `aut`.
**Procedure calls** are of the form `expr`, e.g.,
```
(print (string "Hello World"))
```
where `print` is a function with a `void` return type (i.e., a procedure).

## Expressions
An expression is a either a *token* or a *function application*. **Token** is the same as defined in [.vtf syntax](README.md) and **function application** is of the form
```
(func-name arg1 arg2 ... argN)
```
where `func-name` is the function name and `arg1`, `arg2`, ..., `argN` is a list of positional arguments, which are also expressions.

The particular function to be applied is determined by `func-name` and the number and types of the arguments.
Functions can be *variadic*, i.e., the number of their arguments is not fixed.
In case a fixed-arity function and a variadic function with the same `func-name` are present, the fixed-arity has priority.

## Types
The type system is dynamic.
Every expression has a type determined during execution.
Types are either
* **basic**: lower-case, e.g. `void`, `bool`, or `string`, or
* **complex**: upper-case, e.g. `NFA`, `NTA`, or `STATE-REL`.

The dynamic typing should allow domain-agnostic operations to be easily implemented.
For instance, the following snippet of code
```
aut3 = (union (load_aut "aut1.vtf") (load_aut "aut2.vtf"))
```
should compute a representation of the union of `aut1.vtf` and `aut2.vtf` without a prior knowledge what is the domain of the automaton model in `aut{1,2}.vtf`, e.g., whether the automaton is over finite/infinite words, trees, etc.

## Examples of Use
This section gives some intended examples of use

### A "Hello World" Example
```
@CODE
(print (string "Hello World"))
```

### Test Inclusion
```
@CODE
(start-timer timer1)
incl = (is_incl (load_aut "aut1.vtf" "aut2.vtf"))
(stop-timer timer1)
(println (string "Inclusion: ") incl (string "\nTime: ") timer1)
```

### Test Inclusion with Antichains and Simulation
```
@CODE
aut1 = (load_aut "aut1.vtf")
aut2 = (load_aut "aut2.vtf")
sim = (direct-sim aut1 aut2)
(start-timer timerInc)
incl = (is_incl aut1 aut2 "ac+sim" sim))
(stop-timer timerInc)
(println (string "Inclusion: ") incl (string "\nTime: ") timerInc)
```

### Test Inclusion the Old-Fashioned Way
```
@CODE
aut1 = (load_aut "aut1.vtf")
aut2 = (load_aut "aut2.vtf")
aut2c = (complement aut2)
autisect = (isect aut1 aut2c)
isect_empty = (is_empty autisect)
(println (string "Inclusion holds: ") isect_empty)
```

## Future work
* support definition of macros
* support limited loop-free branching
