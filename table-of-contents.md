# Table of Contents

This table of contents serves as an overview of this course and index for more specific topics and where they are covered.

## Part 1 - Introduction

These first few chapters introduce some basic data types and cover most of the fundamental concepts, features, and syntax in Agda.
The idea of Proofs-as-Programs is teased and lightly explored along with the power and expressiveness of Agda's type system.
Exercises primarily build comfort with Agda syntax and interactive editing, while providing intriguing challenges that could (for the most part) be tackled in any modern programming language.

### Ch0 - Functions

- Basic OPTIONS and top-level modules
- Unicode support and interactive editing
- Polymorphic functions
- Normal vs implicit/inferred parameters
- Currying
- Lambdas
- Higher order functions
- Absurdities

### Ch1 - Booleans

- "import", "open", and "open import"
- The definition of Bool
- Pattern matching with Bool
- Infix operators, precedence, and confusing errors
- Evaluating/normalizing expressions
- Type aliases, functions into Set, and compile-time computation
- Church-encoded Bool

### Natural Numbers

- open import using (...)
- The definition of the natural numbers
- Pattern matching on natural numbers
- Recursion and termination checking
- BUILTIN pragmas
- Inductive reasoning about correctness
- Church-encoded Nat

## (WIP Table of not-yet-completed Contents)

This section is just for me, so I can develop an overall plan and save ideas for later.

### Products

- Simple records
- A generalized Pair type
- Levels and universe polymorphism
- Dependent records
- (Raw) Magmas, Semigroups, Monoids, Groups
- Simple dependent pattern matching
- Sigma

### Typeclasses

- Instance arguments
- Eq
- Ord
- Enum
- Ix

## Part 2 - Functional Programming

With a firm grasp over all of Agda's basic features, we dive into functional programming in Agda.
This part should leave the reader with a firm understanding of all the most central themes that might be found in a typical haskell tutorial.

### Maybe

- Lazy evaluation in Agda

### List

- Correct-by-construction code

### Sums

### Unit and Void

- Unit
- Void
- Negation
- Degenerate forms of polymorphic functions

### Effects

- (Raw) Functors
- (Raw) Applicatives
- (Raw) Monads
- Reader
- Writer
- State

## Part 3 - Dependent Types

### Dependent Functions

### Recursors and Eliminators

- natural-induction
- Free theorems from parametricity, limitations of church encodings

### GADTs

- Reflects

## Part 4 - Equality

### Propositional Equality

- Leibnitz equality
- The identity type

### Decideable Equality

### Extensionality

## Part 5 - Programming with Types

### Finite Sets

### Length-indexed Lists

- Bit arrays and operations

### Termination

- Well founded relations

## Part 6 - Mathematical Constructs

### Isomorphisms

- Cardinalities and operations on finite sets

### Abstract Algebra

- Semigroup
- Monoid
- Group

### Setoids

### Double-Negation

- Double negation elimination
- Law of excluded middle
- Double negation shift

## Part 7 - Category Theory

### Categories

### Functors

### Natural Transformations

### Monads

### Monad Transformers

## Part 8 - Infinity

### Infinite Series

### Coinduction

- --guardedness
- Stream
- Colist
- Delay
- Conat
- Bisimilarity

### Cardinalities

- Non-invertible functions
- type-in-type paradox?

## Part 9 - Cubical Agda

### Paths

- Proof of extensionality

### HITs

### Quotients

### Univalence

### Primitives

- Using primitives to define the functions we used in the previous chapters on Cubical

## Part 10 - Meta

### The IO Monad

### Monadic Parsing

### Propositional Logic

### Lambda Calculus

### Predicate Logic

### Peano Arithmetic
