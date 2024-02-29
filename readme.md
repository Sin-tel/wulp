Proof-of-concept scripting language that transpiles to lua

### Backend 
* Luajit fast
* Lua code is easy to generate
* Using luajit as a backend is a viable strategy for quickly iterating on a language
* I started this project by just forking an existing lua parser, and changing the syntax to fit my personal tastes. This was overall very painless (except when i struggled for three days to implement the type checker)
* The emitted lua code is not necessarily idiomatic, but fairly readable. It should play nice with jit heuristics.

### Strong typing
A half-decent implementation of Hindley-Milner inference basically allows you to omit type annotations in most places while still getting all of the compile time guarantees.
It also gets you generics for free. 

I did not bother with subtyping, unions or flow-control based checking (a la mypy or typescript), since these are very hard to make watertight.
Although I believe it would not be too hard to add some implicit coercions where it makes sense.

### Syntax
Some notable changes from lua:

* Use `{}` instead of `do`, `then`, `end` etc
* Using an undeclared variable is a compile time error instead of silently making a global
* Added `+=` and friends
* Arrays are zero indexed
* No metatables. I consider being able to arbitrarily override things a weakness, since it mostly just leads to idiosyncratic code (see: the hundreds of different ways people have reinvented OOP in lua).
The current syntax aims to codify a few common idioms, giving you only one way of doing things.

### TODOs
* Standard library needs to be filled out
* Some way to deal with optional types
* I want to add a trait/typeclass system eventually
* IDE integration, autoformatting etc
* Eventual goal is to integrate nicely with love2d
