{-# OPTIONS --safe --cubical-compatible #-}

module Exercises.Ch2 where

{-

This chapter will define the natural numbers as a data type and explore how we can use them to perform recursion.

We will start off again by importing some of what we have defined in previous chapters, but we need to start being a bit more orderly.

So, imports for all future chapters will be managed in a separate folder, "Imports", which you should feel welcome to inspect.

To avoid cluttering the global namespace, we will prefer to only import more important functions, like ones that you might find in the stdlib.

If you ever want to import something else though, you shouldn't hesitate to do so.

-}

open import Imports.Ch2

{-

Please briefly read through the above file.

Now, brace yourself, as we are about to define the natural numbers:

-}

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- ℕ can be typed as \bN
-- However here is an ascii synonym, as promised:
Nat = ℕ

-- In ADT syntax, this definition is equivalent to:
-- data Nat = Zero | Suc Nat

{-

This declares ℕ, the natural numbers, which are Set, and furthermore the two distinct ways of constructing a natural number.

Firstly, "zero" is a natural number.

Furthermore, "suc" is a constructor that takes a natural number, and returns a new, distinct natural number.

"suc" is short for "successor", which is a mathematical term for referring to the function "plus 1" aka "increment"

If this is your first time seeing the natural numbers this way, you are probably pretty confused, or at least taken aback.

In a more traditional programming language, this definition might be written as something like:

  abstract class Nat

  class Zero extends Nat

  class Suc extends Nat {
    Nat pred;
  }

This is comparable to a singly linked list, except that our "Node" (i.e. Suc) doesn't contain an element in addition to the tail of the list.

So we effectively have a dummy linked list, whose length corresponds to the natural number which we are trying to represent.

So, to write a given natural number, "n", you just have to apply n "suc"s to zero, i.e. increment zero n times.

Try defining a few numbers this way:

-}

one : ℕ
one = {!   !}

two : ℕ
two = {!   !}

three : ℕ
three = {!   !}

seven : ℕ
seven = {!   !}
-- Don't worry if this seems tedious, we will soon explore how to write large numbers with more reasonable syntax.

boolToBitValue : Bool → ℕ
boolToBitValue false = zero
boolToBitValue true = suc zero

{-

This is a very mathematical way of defining the natural numbers, and it is very effective when we don't want to deal with bytes and memory.

Although it may not be very efficient, it has a very simple structure, and doesn't rely on complex/low-level code, making it easy to reason about.

By defining this type recursively we also manage to acheive a type/Set with infinitely many elements, so we aren't limited to 32 or 64 bits.

There are a few properties of this definition that makes it a *real* definition of the natural numbers, which are worth noting.

For one, there is no instance of this datatype that does not correspond to an actual natural number.

However, and with equal importance, there is exactly one way to represent any given natural number with this datatype.

It is worth pausing to convince yourself of these two facts. Also keep in mind the following implicit laws of these constructors:
  zero and suc are disjoint, so ∀ n, zero ≠ suc n
  suc is a function, so ∀ n m, n = m implies suc n = suc m
  suc is injective, so ∀ n m, suc n = suc m implies n = m

Now, lets define some simple functions on ℕ:

-}

-- Until we have defined general addition, we can use mixfix to define some cheeky syntax for adding particular numbers.
-- This allows us to write things like "3+ 2+ 3+ 1+ f x" instead of having to write "3+ (2+ (3+ (1+ (f x))))"
infixl 6 0+_ 1+_ 2+_ 3+_

0+_ : ℕ → ℕ
0+ n = {!   !}

1+_ : ℕ → ℕ
1+ n = {!   !}

2+_ : ℕ → ℕ
2+ n = {!   !}

3+_ : ℕ → ℕ
3+ n = {!   !}

-- Use this as a chance to play around with the above definitions, and remember to use C-c C-n to test/evaluate expressions
ten : ℕ
ten = {! 3+ 2+ 3+ 1+ suc zero  !}

{-

This is fine but we also should be able to do more than just increment numbers, and this doesn't seem to be possible using just zero and suc.

We can in fact accomplish this however, by pattern matching, just like we have done with booleans.

Natural numbers, are a bit more interesting to pattern match with, because the suc case contains another natural number (its predecessor).

In a traditional language, simple pattern matching on ℕ like would be like declaring a virtual method in the base class "Nat".

Then by defining the method in each derived class, you handle the "Zero" case and the "Suc" case where you have another "Nat"

and handeling both cases by overring that method.

Give this a try, once again with C-c C-c:

-}

isZero : ℕ → Bool
isZero n = {!  !}

isPositive : ℕ → Bool
isPositive n = {!   !}

-- The "predecessor" function
-- Should subtract 1 from the input
-- This is obviously not possible for zero, so in this case it should just return zero.
pred : ℕ → ℕ
pred n = {!   !}

{-

When we case split on a natural number, we introduce another natural number in the suc case, in the form "(suc other-natural-number)".

This other natural number is, of course, one less than the original natural number, aka the "predecessor".

You can use this variable just like any other argument, and you can even case split on it again.

For readability, it can be worth renaming this variable from whatever name Agda automatically gives it, perhaps to something like n' or pred-n.

Don't forget though, to reload after modifying anything by hand, so that interactive commands are aware of any changes you have made.

Try using this knowledge to define the following functions:

-}

isOne : ℕ → Bool
isOne n = {!   !}

isTwo : ℕ → Bool
isTwo n = {!   !}

isThreeOrFour : ℕ → Bool
isThreeOrFour n = {!   !}

{-

As you can see, these cases can become nested, and to any arbitrary depth, and are called "patterns", hence "pattern matching"

You can of course also write these patterns yourself, as long as you are careful about the parentheses.

You can also write overlapping cases, i.e. with a default case at the end like:
  foo (...) = ...
  foo (...) = ...
  foo (...) = ...
  foo x = ...

-}

isZeroOrNine : ℕ → Bool
isZeroOrNine n = {!   !}

isBetweenFiveAndEight : ℕ → Bool
isBetweenFiveAndEight n = {!   !}

{-

You may be wondering however, how we can define more complicated operations like addition or multiplication.

The techniques we have used until now are not powerful enough to do so, but luckily we have one more, incredibly powerful tool, recursion.

Being a pure functional language, Agda doesn't have for-loops, but we can compensate for this by writing recursive definitions.

However, there is an additional restriction to keep in mind, namely that our recursive functions must be provably total.

This is a fancy way of saying that our functions must terminate for all inputs, as this will be crucial when we start proving theorems.

For instance, the following definition is not allowed:

-}

-- bad : ℕ → Bool
-- bad n = bad (pred n)

{-

Try commenting it back in to see what kind of errors we get.

This function clearly never terminates, so it's nice that the termination checker bans its definition.

Note:
  In fact, one way for us to see that it must not terminate, is the fact that the definition would type check with any return type.
  It claims to construct a Bool (or whatever type you replace Bool with), without ever actually doing so.
  So, if this were a valid total function definition, we could use it to "construct" an element of any type, which is absurd.

However, sometimes the termination checker will complain about a completely valid function definition:

-}

-- still-bad : ℕ → Bool
-- still-bad n = if isZero n then true else okay (pred n)

{-

The above recursive function is kind of stupid for demonstration purposes, as it pointlessly recurses over its input and then always returns true.

The point however, is that this function clearly terminates, recursively decrementing a natural number until it reaches zero, and then returning true.

On the other hand, however, the termination checker doesn't see this definition any differently than the first example.

In fact, as humans we are taking quite a bit of reasoning for granted, just because it is intutive to us. Namely:
  1. If you apply pred to a natural number enough times, it will eventually reach zero
  2. When it reaches zero, "isZero x" will return true
  3. if_then_else_ will not use "okay (pred x)" if "isZero x" is true

For the termination checker, this would require an arbitrary amount of looking at definitions and brute force searching.

In later chapters we will learn how to write such a definition and prove that it terminates, but until then this is actually completely unnecessary.

We can actually write the majority of functions in a way that makes it clear (for us, as well as for the termination checker) that they terminate.

We can do this as follows:

-}

good : ℕ → Bool
good zero = true
good (suc n) = good n

{-

Here, we use pattern matching to not only (arguably) make the definition more readable, but also make it clear that it terminates.

By the nature of data type definitions in Agda, a pattern like (suc x) will always be "structrually larger" than x.

Every constructor in Agda produces a value that, naturally, is structurally larger than all of its inputs.

So, when pattern matching, effectively any subexpression of a pattern is structurally smaller than the entire pattern.

The termination checker just checks that at every recursive call, at least one argument is always getting structurally smaller.

In the case of ℕ, the structural size of a natural number corresponds exactly to how large it is.

This is another advantage of defining ℕ recursively as we have done: we get provably terminating recursion for free.

Now, lets write some recursive functions that actually do something.

Please experiment with different techniques for writing these definitions, and remember to test out your definitions.

-}

infix  4 _≡ᵇ_ _==b_
infixl 6 _+_
infixl 7 2*_

isEven : ℕ → Bool
isEven n = {!   !}

isOdd : ℕ → Bool
isOdd n = {!   !}

2*_ : ℕ → ℕ
2* n = {!   !}

_+_ : ℕ → ℕ → ℕ
n + m = {!   !}

-- Boolean equality, written with \==\^b
-- Here the asci-synonym also has its type declared so you can use it for recursive calls if you don't like typing unicode.
--   (Technically, this is mutual recursion, which is actually well understood by the termination checker)
_≡ᵇ_ _==b_ : ℕ → ℕ → Bool
n ≡ᵇ m = {!   !}

_==b_ = _≡ᵇ_

{-

If termination checking is ever too restrictive, you can disable --safe and just (unsafely) label functions as {-# TERMINATING #-}.

Before we look at more exercises, lets simplify the writing and display of natural numbers:

-}

{-# BUILTIN NATURAL ℕ #-}

{-

The above magical declaration says the following to Agda:

"Hey look, my definition of "ℕ" resembles the classic recursive definition of the natural numbers. Please give me syntactic sugar for it"

And so, with this BUILTIN pragma, Agda will now automatically translate number literals to and from chains of zero and suc.

Other than being an overall quality of life improvement, this lets us write definitions that would have been impractical before:

-}

plusOneThousand : ℕ → ℕ
plusOneThousand n = {!   !}

{-

This pragma also causes Agda to store natural numbers much more efficiently, so you don't run out of memory while performing computations.

We can also tell Agda about our definitions of certain operations (like addition), so Agda can replace them with faster primitive operations.

However, Agda must be able to recognize that these operations are correct, and will give you an error if it cannot do so.

So, feel free to comment these lines in once you have correctly (and simply) defined the referenced operations.

-}

-- {-# BUILTIN NATPLUS _+_ #-}


-- Since ≡ᵇ returns a Boolean, we also need to declare our definition of Bool to Agda before we bind it.
{-# BUILTIN BOOL  Bool  #-}
-- Unlike with zero and suc, which have different types, Agda technically cant infer which elements of Bool are meant to be true or false
{-# BUILTIN TRUE  true  #-}
{-# BUILTIN FALSE false #-}


-- {-# BUILTIN NATEQUALS _≡ᵇ_ #-}


{-

This trick allows us to enjoy the structural clarity of the recursively defined natural numbers, without sacrificing performance.

Additionally, If you are ever writing Agda without the standard library, Agda comes with files containing pre-bound BUILTIN definitions.

These can be imported from the modules Agda.Builtin.Bool, Agda.Builtin.Nat, Agda.Builtin.Int etc.

Compared to traditional programming languages, this design might seem a bit overkill for just writing programs.

However the simplicity and clarity of recursively defined natural numbers is an amazing help when reasoning about properties of functions.

As we use more and more advanced features of Agda's type system, eventually writing real proofs, this will become very clear.

Lets define some more functions.

-}


infix  4 _<ᵇ_ _≤ᵇ_
infixl 6 _∸_ _-_ _⊔_ _max_
infixl 7 _*_ _⊓_ _min_

-- Subtraction rounded up to zero, written as \.-
_∸_ _-_ : ℕ → ℕ → ℕ
n ∸ m = {!   !}

_-_ = _∸_

_*_ : ℕ → ℕ → ℕ
n * m = {!   !}

-- Boolean less-than, written <\^b
_<ᵇ_ _<b_ : ℕ → ℕ → Bool
n <ᵇ m = {!   !}

_<b_ = _<ᵇ_

-- {-# BUILTIN NATMINUS _∸_ #-}
-- {-# BUILTIN NATTIMES _*_ #-}
-- {-# BUILTIN NATLESS _<ᵇ_ #-}

-- Continue to mess around and try writing both efficient (defined in terms of builtins) and inefficient (recursive) definitions

-- Maximum, written \lub
_⊔_ _max_ : ℕ → ℕ → ℕ
n ⊔ m = {!   !}

_max_ = _⊔_

-- Minimum, written \glb
_⊓_ _min_ : ℕ → ℕ → ℕ
n ⊓ m = {!   !}

_min_ = _⊓_

_≤ᵇ_ _<=b_ : ℕ → ℕ → Bool
n ≤ᵇ m = {!   !}

_<=b_ = _≤ᵇ_

{-

A lot of these operations seem to come down to more or less just doing something n times.

In fact, the ability to do something n times is actually all we technically need to perform any arbitrary computation with ℕ.

In practice this can be sometimes very hard without access to other useful datatypes, but for simple recursive functions it's often pretty doable:

-}

applyNTimes : ℕ → {A : Set} → (A → A) → A → A
applyNTimes n f x = {!   !}

_+'_ : ℕ → ℕ → ℕ
n +' m = applyNTimes {!   !} {!   !} {!   !}

_∸'_ : ℕ → ℕ → ℕ
n ∸' m = applyNTimes {!   !} {!   !} {!   !}

_*'_ : ℕ → ℕ → ℕ
n *' m = applyNTimes {!   !} {!   !} {!   !}

{-

Just like with the booleans, we can also compute types with the natural numbers, with pattern matching and recursion:

-}

-- The type of a function which takes n arguments of type A and returns an R
NaryFunction : ℕ → Set → Set → Set
NaryFunction n A R = {!   !}

-- a synonym for _≡ᵇ_
binary-equals : NaryFunction {!   !} {!   !} {!   !}
binary-equals = {!   !}

-- a synonym for suc
unary-suc : NaryFunction {!   !} {!   !} {!   !}
unary-suc = {!   !}

-- a synonym for 12
twelve : NaryFunction {!   !} {!   !} {!   !}
twelve = {!   !}

{-

Finally, before moving on to the rest of the exercises, lets explore something beautiful about the nature of recursion:

When we reason about the correctness of recursive functions, we are actually using induction (over the natural numbers).

The principle of induction in mathematics states that:

  Given a predicate P over the natural numbers,

  if P(0) holds, (this is often called the base case)

  and P(n) implies P(n+1), (this is often called the inductive step)

  then P holds for all natural numbers.

If you squint a bit, this actually looks extremely similar to how we recurse over ℕ.

Take the following function for instance:

-}

infixr 8 2^_

2^_ : ℕ → ℕ
2^ zero = 1        
2^ suc n = 2* 2^ n 

{-

How do we know that this is indeed a valid definition of the same 2ⁿ function that we know from math class? By induction.

Specifically, where P(n) is the proposition that "2^ n ≡ 2ⁿ" (i.e. the one we have defined in Agda equals one that we know from math class)

Then, the base case is:

  "2^ 0 ≡ 2⁰"

and the inductive step is:

  "2^ n ≡ 2ⁿ implies 2^ (n + 1) ≡ 2ⁿ⁺¹"

And amazingly, these proofs are both completely trivial, thanks to how we defined 2^_.

The base case holds because "2^ 0" is defined to be equal to "1", and we know from math class that "2⁰ ≡ 1".

The inductive step holds because, by definition "2^ (n + 1) ≡ 2* 2^ n", and we get to assume that "2^ n ≡ 2ⁿ", so "2^ (n + 1) ≡ 2* 2ⁿ".

  (notice how the correctness of the recursive call is exactly what we get to assume in the inductive step)

Finally, we know from math class that "2 × 2ⁿ ≡ 2ⁿ⁺¹", and so we have "2^ (n + 1) ≡ 2ⁿ⁺¹", proving the inductive step.

The only thing missing is a proof that your definition of "2* n" is the same as "2 × n" from math class, which I leave to the reader.

So, we have effectively written a program for raising two to the power of a number, merely by specifying the *laws* of this operation.

The inductive reasoning used was entirely trivial, and it follows automatically from the fact that we wrote this definition recursively.

This beautiful idea can absolutely also be found in other languages, even imperative ones, with for-loops and such.

However the beauty is perhaps exemplified or made clearer by the clean way it can be done with recursion and pattern matching.

Furthermore, this kind of reasoning does not only work with natural numbers, but rather with every data type you could possibly define.

Every datatype has its own unique form(s) of induction, given for free by way(s) that we can properly recurse over it.

With ℕ, for instance, we can recurse with a different form, corresponding to a different (but equivalent) formulation of induction, i.e.

  if P(0) and P(1) and P(n) => P(n+2), then P holds for all natural numbers.

Which would correspond to pattern matching and recursing like

foo zero = ...
foo (suc zero) = ...
foo (suc (suc n)) = ... foo n ...

We will explore this idea more deeply in coming chapters.

-}


-- practice exercises:


infixr 8 _^_
infix  8 _!

-- Division by 2, rounded down, written with \lfloor and \rfloor
⌊_/2⌋ halve : ℕ → ℕ
⌊ n /2⌋ = {!   !}

halve = ⌊_/2⌋

-- Division by 2, rounded up, written with \lceil and \rceil
⌈_/2⌉ halveRoundedUp : ℕ → ℕ
⌈ n /2⌉ = {!   !}

halveRoundedUp = ⌈_/2⌉

-- Exponentiation
_^_ : ℕ → ℕ → ℕ
x ^ n = {!   !}

-- Factorial
_! : ℕ → ℕ
n ! = {!   !}

-- Summation of a function up to a bound
sum-of_until_ : (ℕ → ℕ) → ℕ → ℕ
sum-of f until n = {!   !}


-- Define the following functions using applyNTimes instead of explicit recursion:

isEven' : ℕ → Bool
isEven' n = {!   !}

isZero' : ℕ → Bool
isZero' n = {!  !}

_<'_ : ℕ → ℕ → Bool
n <' m = {!   !}

_≡ᵇ'_ : ℕ → ℕ → Bool
n ≡ᵇ' m = {!   !}


-- applyNTimes returns a "{A : Set} → (A → A) → A → A", which is actually the type of the church encoding of natural numbers.
-- Just like pick and unpick with bool, this type is (theoretically) exactly as powerful to ℕ in terms of what it can compute.

church-to-nat : ({A : Set} → (A → A) → A → A) → ℕ
church-to-nat n = {!   !}

-- Try defining each of the following functions as pure lambda terms, without using any other definitions.

church-zero : ({A : Set} → (A → A) → A → A)
church-zero = {!   !}

church-suc : ({A : Set} → (A → A) → A → A) → {A : Set} → (A → A) → A → A
church-suc n = {!   !}

church-plus : ({A : Set} → (A → A) → A → A) → ({A : Set} → (A → A) → A → A) → {A : Set} → (A → A) → A → A
church-plus n m = {!   !}

church-times : ({A : Set} → (A → A) → A → A) → ({A : Set} → (A → A) → A → A) → {A : Set} → (A → A) → A → A
church-times n m = {!   !}

church-exp : ({A : Set} → (A → A) → A → A) → ({A : Set} → (A → A) → A → A) → {A : Set} → (A → A) → A → A
church-exp n m = {!   !}

-- Stream A is an infinite series or infinite list of A's
Stream : Set → Set
Stream A = {!   !}

-- Returns the first element of a stream
head : {A : Set} → Stream A → A
head stream = {!   !}

-- Returns all but the first element of a stream
tail : {A : Set} → Stream A → Stream A
tail stream = {!   !}

-- Chops off the first n elements of a stream
drop : {A : Set} → ℕ → Stream A → Stream A
drop n stream = {!   !}

-- Repeats a value infinitely
repeat : {A : Set} → A → Stream A
repeat x = {!   !}

-- Applies a function to each element of a stream
map : {A B : Set} → (A → B) → Stream A → Stream B
map f stream = {!   !}

-- Zips two streams together with an operation
zipWith : {A B C : Set} → (A → B → C) → Stream A → Stream B → Stream C
zipWith op stream1 stream2 = {!   !}

-- Reduces/folds a stream over an operation, collecting each intermediate result
-- for example, "scan _+_ 5 [1, 3, 0, 2 ...]" would result in [6, 9, 9, 11 ...]
scan : {A B : Set} → (B → A → B) → B → Stream A → Stream B
scan op x stream = {!   !}


{-

open-ended exercises:

Use induction to prove some properties about your definitions, such as:
  ∀ n, isEven (2* n) ≡ true
  ∀ n, 1 + n ≡ n + 1
  ∀ n m, n + m ≡ n +' m
  ∀ n m, n * m ≡ m * n
  ∀ n m, (n < m) ∧ (m < n) ≡ false

Define ℤ (aka Int) as a recursive data type like ℕ, along with some operations.
  Make sure there is exactly one way to represent each Integer.

Define a recursive data type which can represent expressions of the SKI combinator calculus.
  Can you write a function to fully reduce an SKI expression?

Define a data type with no "base case", and only a "recursive case".
  Can you construct an instance of this type?
  If not, can you show that an instance of this type is absurd (i.e. that it implies an absurdity)?

-}



-- challenge exercises:
     

-- Infinite binary sequences (of the form "Stream Bool") are known to be unenumerable.
-- Given a purported enumeration of them (of the form "ℕ → Stream Bool"), construct a counter-example which it does not contain.
infinite-sequences-are-unenumerable : (ℕ → Stream Bool) → Stream Bool
infinite-sequences-are-unenumerable enumeration = {!   !}

-- Flattens an infinite stream of infinite streams into a single infinite stream.
-- The only requirement is that the result contains every element of every sub-stream of the input at some point.
flatten-streams : {A : Set} → Stream (Stream A) → Stream A
flatten-streams stream-stream = {!   !}


-- Try to define the following functions on church encodings once again as pure lambda terms.
-- Hint: it may help to explicitly supply type parameters i.e. n {...} (...) (...)

-- Apply n times, or until the given church-boolean predicate returns true
church-apply-until-true : ({A : Set} → (A → A) → A → A) → {A : Set} → (A → A) → A → (A → {B : Set} → B → B → B) → A
church-apply-until-true n = {!   !}

-- Wierdly, this is much harder to define with church encoded naturals.
church-pred : ({A : Set} → (A → A) → A → A) → {A : Set} → (A → A) → A → A
church-pred n {A} = {!   !}

-- Division by two, rounded down
church-halve : ({A : Set} → (A → A) → A → A) → {A : Set} → (A → A) → A → A
church-halve n {A} = {!   !}


-- Division and Modulo with natural numbers are actually a bit tricky to define with termination checking.
-- One way to do so is to define a helper function, which we will call div-helper.
-- div-helper should be defined so that: n / (1 + m) = div-helper 0 m n m
-- Hint: only pattern match on the last two arguments, not the first two.

div-helper : ℕ → ℕ → ℕ → ℕ → ℕ
div-helper k m n j = {!   !}

-- The idea for Modulo is very similar.
mod-helper : ℕ → ℕ → ℕ → ℕ → ℕ
mod-helper k m n j = {!   !}

-- Using BUILTIN pragmas for these helper functions, we can now define fast division and modulo operations

-- {-# BUILTIN NATDIVSUCAUX div-helper #-}
-- {-# BUILTIN NATMODSUCAUX mod-helper #-}

-- since we don't yet know how to require that an argument be non-zero, we can simply define slightly different functions:

_/[1+_] : ℕ → ℕ → ℕ
n /[1+ m ] = div-helper 0 m n m

_%[1+_] : ℕ → ℕ → ℕ
n %[1+ m ] = mod-helper 0 m n m