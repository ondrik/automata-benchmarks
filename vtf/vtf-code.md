# Proposal for VTF `@CODE` semantics
The basic syntax of `@CODE` sections is according to the [basic .vtf syntax](README.md), i.e., the section begins with a line with `@CODE` and a sequence of lines until the next `@`-header or end of file follows.
The lines can start with `%`, in which case they are *meta* lines, or not, in which case they are body lines.
The format is line-oriented.
This document describes the syntax and semantics of a `@CODE` section in more detail.

TODO

### A "Hello World" Example
```
@CODE
(print (string "Hello World"))
```
