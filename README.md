# BF-HS: A Haskell Implementation of the BF Language

Welcome to BF-HS, a Haskell-based implementation of the esoteric programming language commonly abbreviated as "bf". While the original language name contains profanity, it's important to recognize that the concept behind the language is a fascinating exploration of minimalistic design in programming languages.

## Why BF-HS?

This project serves as an exercise (a kata) to refine my Test-Driven Development (TDD) skills. It provides a unique challenge in implementing and testing a language interpreter with a very limited set of commands.

## Getting Started

I derived a little from original BF specs. I use infinite tape of 10 cells, not 30000, please extend it if you want to. It is also a tape of Ints, not chars, so just simple arithmetic computations work. This can be easily modified locally. The program is also just a string, it doesn't read from a file, even though it's another simple change. Perhaps it could be a fun excericise to make it read from a file, and to be able to specify the size and type of a runtime tape.

### Prerequisites

Ensure you have the following installed:
- GHC (Glasgow Haskell Compiler)
- Cabal

### Running BF-HS

To run a BF program using this interpreter, you can use the executable built from the source. First, ensure you build the project:

- cabal build to build the project
- cabal run to run the interpreter
- cabal test to run the tests

