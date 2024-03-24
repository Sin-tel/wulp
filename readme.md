# Wulp

Scripting language that compiles to lua.

### Syntax

<img src="syntax.png" alt="syntax" style="width:763px;"/>

Some notable changes from lua:

* Use `{}` instead of `do`, `then`, `end` etc
* Using an undeclared variable is a compile time error instead of silently making a global
* Added `+=` and friends
* Arrays are zero indexed
* No metatables. The current syntax aims to codify a few common idioms, giving you only one way of doing things.

See the [examples](wulp) for more.

There is syntax highlighting and completions package for Sublime text in the [sublime](sublime) folder.

### Lua backend 
* Using luajit as a backend is a viable strategy for quickly iterating on a language
* Lua code is easy to generate
* Luajit fast
* I started this project by just forking an [existing](https://github.com/sbdchd/luis/) lua parser, and changing the syntax to fit my personal tastes. This was overall very painless (except when i struggled for three days to implement the type checker)
* The emitted lua code is not necessarily idiomatic, but fairly readable. It should play nice with jit heuristics.

### Type system
A half-decent implementation of Hindley-Milner inference basically allows you to omit type annotations in most places while still getting all of the compile time guarantees.
It also gets you generics for free. 

I did not bother with subtyping, unions or flow-control based checking (à la mypy or typescript), since these are very hard to make watertight.
It would not be too hard to add some implicit coercions where it makes sense.


### TODOs
* Standard library needs to be filled out
* Some way to deal with optional types
* I want to add a trait/typeclass system eventually
* IDE integration, autoformatting etc
