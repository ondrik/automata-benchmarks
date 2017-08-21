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

We do not force functions to have a fixed number of arguments.

## Types
Every expression has a type, which are:
* **basic**: e.g. `void`, `bool`, or `string`
* **complex**: e.g. `NFA`, `NTA`, or `STATE-REL`

### A "Hello World" Example
```
@CODE
(print (string "Hello World"))
```
