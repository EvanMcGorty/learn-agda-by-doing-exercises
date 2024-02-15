{-# OPTIONS --safe --cubical-compatible #-}

{-

This file will define an actual concrete data type, the booleans, and explore what kinds of functions we can write with them.

We will also learn how to define aliases for types and write some basic type level functions.

Once again, we will start out with a module declaration.

However this time, we will also import everything from the previous chapter.

-}

module Exercises.Ch1 where


open import Solutions.Ch0

{-

"open import XYZ" is syntactic sugar for "import XYZ" followed by "open XYZ"

Importing a file brings its module into scope, so we can access its definitions with "MyModuleName.myDefinition"

"open"-ing it simply allows us to refer to symbols without prefixing them like this, i.e. simply "myDefinition"

The "open"-ness of a module is local (by default). So if someone opens Ch1, Ch0 would not be open to them.

Now get ready for a definition:

-}


data Bool : Set where
  false : Bool
  true : Bool
-- In Agda we use whitespace instead of tabs, and we generally indent in increments of two spaces
-- Your editor should be smart enough to handle this automatically when you press tab

-- If you know ADT syntax like in Haskell or ML, this definition is equivalent to:
-- data Bool = False | True

{-

This definition declares a data type, "Bool", which is a Set (i.e. a type).

Furthermore, it declares that there is a Bool named "false", and a Bool named "true".

false and true are called constructors, as they are ways to construct a Bool.

In Agda, no data types are automatically baked directly into the language, so we either have to define to define them by hand or use a library.

So, this is kind of like manually writing the following definition in a more typical programming language:

  abstract class Bool

  class False extends Bool

  class True extends Bool

It is important to understand that Bool cannot be extended any further, so we can assume false and true are the *only* values of type Bool.

It is also important to understand that Bool is *only* a datatype, and has nothing to do with proofs and propositions in Agda.

Its only purpose is for computation, i.e. we could have very well named it "Bit" with constructors "off" and "on".

Now that we have an actual concrete data type, we can actually define functions that aren't polymorphic.

Lets look at some ways to define the identity function for Booleans.

Reference the previous chapter as necessary, and try to make use of interactive commands.

-}

-- The simple way, just like in the last chapter
idBool0 : Bool → Bool
idBool0 x = {!   !}

-- Defined in terms of idBool0
idBool1 : Bool → Bool
idBool1 x = {!   !}

-- Simply equal to idBool0 (thanks to eta-equivalence)
idBool2 : Bool → Bool
idBool2 = {!   !}

-- Defined in terms of id, with an explicit type parameter
idBool3 : Bool → Bool
idBool3 x = id {{!   !}} {!   !}

-- Simply equal to id, with an explicit type parameter
idBool5 : Bool → Bool
idBool5 = id {{!   !}}

-- Defined in terms of id, but taking advantage of type inference
idBool4 : {!   !} → Bool
idBool4 x = id x

-- Simply equal to id, but taking advantage of type inference
idBool6 : {!   !} → Bool
idBool6 = id

{-

It is worth noting that these identity functions aren't really meaningful propositions/proofs, as Bool itself isn't a meaningful proposition.

Interpreting types as propositions is something which *we* may choose to do, but this notion of propositions is not baked into Agda.

In this sense, types-as-propositions is an idea within our minds, whereas Bool is a concrete thing in Agda, regardless of their similarities.

Bool is just a data type, or a Set with two elements. It is trivial to construct a Bool, which is what it would mean to "prove Bool".

So, if anything, since it is a nonempty Set, we could interpret Bool to be a trivially true proposition. But it's really just a datatype.

Moving on, lets define some more interesting operations on booleans:

-}

not : Bool → Bool
not x = {!   !}

{-

In order to define this function, we need to perform case analysis, aka pattern matching.

This can be done by hand, but instead we will use a new interactive command, called case split.

To case split, type an argument which you wish to inspect into the hole, and press C-c C-c.

This allows you to define a result for each constructor defined in the data declaration of the data type.

Note: at this point, C-c C-f and C-c C-b to move back and forth between holes become very helpful, remember to use them

Funnily enough, this even gives us another way to define the identity function:

-}

idBool7 : Bool → Bool
idBool7 false = {!   !}
idBool7 true = {!   !}

{-

In fact, there are many more ways to do this.

When we pattern match, we have to cover all cases. But, one way to cover all cases is to simply use a variable:

-}

idBool8 : Bool → Bool
idBool8 false = {!   !}
idBool8 x = {!   !}

{-

This is also completely valid.

However, it's important to be understand how pattern matching definitions are evaluated in such cases.

When pattern matching, a variable is compared against each case one by one, *top to bottom*.

So, only the *first* matching case is the one which will be chosen, and the rest will be ignored.

However, while both true and false are always considered valid "patterns", a variable binding is as well, and can be used alongside them.

This allows for cases, where the definition isn't actually a true equality, i.e. "idBool8 x = true".

If you find this confusing or undesirable, you can enable the --exact-split option so that Agda warns you about such cases.

Otherwise, it can be useful for writing certain definitions more concisely.

Additionally, if you have a case that is entirely unreachable, Agda will also warn you anyway.

In fact, try creating such an example. I encourage the reader to pause and experiment:

(Hint: you need to manage the cases/patterns yourself, C-c C-c alone won't be enough)

-}

idBool9 : Bool → Bool
idBool9 x = {!   !}

{-

It can however still be very useful to write code without overlapping cases, which can make it easier to reason about the correctness of code.

Another useful interactive command is normalize (a fancy word for "evaluate"), which lets you actually run code.

Simply type C-c C-n (which opens an interactive window) and then type in an expression which you wish to normalize.

If you have a hole available, you can also type the expression into it, and then press C-c C-n.

If you need a new hole, you can simply write a little anonymous definition like "_ = ?", and use the hole generated by reloading the file.

It can be worth setting up a few of these such that they are reusable, to help conveniently test your code. For example, perhaps something like:

currentTestFunction = not

_ = {! currentTestFunction false !}
_ = {! currentTestFunction true !}

Something like this will allow you to test out various functions with minimal typing, by simply changing the "currentTestFunction".

Now, let's move on to some more interesting definitions:

-}

-- Don't worry to much about this, we are just declaring the precedence of our newly defined operators
infixr 6 _∧_ _&&_
infixr 5 _∨_ _||_ _xor_

-- read as "and", typed as \and
_∧_ : Bool → Bool → Bool
x ∧ y = {!   !}

-- read as "or", typed as \or
_∨_ : Bool → Bool → Bool
x ∨ y = {!   !}

_xor_ : Bool → Bool → Bool
x xor y = {!   !}

-- Here are some ascii synonyms, as promised :^)
-- (for such simple definitions with no variables, we don't need an explicit type signature)
_&&_ = _∧_
_||_ = _∨_


{-

Here you probably immediately notice the strange use of underscores. Names with underscores are able to be used with operator-like syntax.

For example, if we defined _foo_bar_baz_, then not only can we write "_foo_bar_baz_ a b c d", but we can also write "a foo b bar c baz d".

This feature of Agda, called "mixfix", is of course more than enough to define some simple infix operators, as we have done above.

We also declared these operators to associate to the right, so "x ∧ y ∧ z" is understood as "x ∧ (y ∧ z)".

We also gave "∧" a higher precedence than "∨", so "w ∨ x ∧ y ∨ z" is understood as "w ∨ (x ∧ y) ∨ z".

Take your time to define the above functions, and experiment with different ways of doing so (i.e. case splitting once vs twice).

We can also use Bool to define new polymorphic functions:

-}

-- "pick false" should return the first supplied "A", "pick true" should return the second supplied "A"
pick : Bool → {A : Set} → A → A → A
pick b = {!   !}

-- should be the inverse of pick
unpick : ({A : Set} → A → A → A) → Bool
unpick f = {!   !}


{-

Notice how we can introduce a generic parameter anywhere we want, i.e. for any Bool, pick returns a generic function.

Likewise, unpick *accepts* a generic function but itself is actually not generic.

unpick will only accept an argument which works for *any* Set, so you cannot supply it a Bool → Bool → Bool, for instance.

Conversely, this means that (the definition of) unpick has the freedom to use this argument with whatever type it wants.

For the last topic of this chapter, we will take a peek at some type-level programming.

In Agda, types are actually just values and can consequently be used wherever you might normally use values.

This means that we can even define functions that return Sets, and use them in type signatures.

The Symbol "Set" is not only used to quantify over generic type-parameters, but is actually itself just another type, like "Bool".

This means that we can write a function which returns a Set, and can use it in a type expression:

-}

MyDependentType : Bool → Set
MyDependentType false = Bool → Bool
MyDependentType true = Bool → Bool → Bool

not2 : MyDependentType {!   !}
not2 = {!   !}

_xor2_ : MyDependentType {!   !}
_xor2_ = {!   !}

idBool10 : MyDependentType ({!   !} ∨ {!   !} ∧ true)
idBool10 = {!   !}

{-

This effectively lets us run arbitrary code during typechecking, which is pretty cool.

In later chapters, we will use this to write some really powerful type-level code.

Even for now, this gives us a very simple and nice way of defining aliases for types:

-}

Bit : Set
Bit = Bool

0bit : Bit
0bit = {!   !}

1bit : Bit
1bit = {!   !}

invertBit : Bit → Bit
invertBit = {!   !}

-- Not only can we return types, but we can also take them as arguments
Predicate : Set → Set
Predicate A = A → Bool

invertPredicate : {A : Set} → Predicate A → Predicate A
invertPredicate p = {!   !}

andPredicate : {A B : Set} → Predicate A → Predicate A → Predicate A
andPredicate p q = {!   !}

{-

Working with aliases is a great example of where interactive editing in Agda shines.

Aliases can be confusing to work with, and Agda provides three variants for displaying type information:
  1. C-c: This is the default variant, which you have used until now
  2. C-y: This specifically evaluates/simplifies all types in a context as much as possible before displaying them
  3. C-u: This specifically avoids as much evaluation/simplification as possible when displaying types

So, if you inspect the context of a hole with C-c C-, and "Predicate A" is confusing, using C-y C-, instead will display this as A → Bool

Alternatively, if something is ever getting simplified to a point that makes it harder to read, C-u C-, might be helpful.

This works for the majority of commands that involve displaying types, not just C-c C-,

It's also worth noting that there are a number of miscellaneous commands which start with C-x, such as C-x C-=, aka "lookup unicode input sequence".

As mentioned previously, this is the reason that Ctrl-x may not function properly as "cut", so you may want to disable/remap them.

Something important to understand about "Set"s is that they are not concrete data, and cannot be inspected from within a function.

We cannot "pattern match" on a type to determine whether it is Bool, Int, or String, nor can we determine really any properties about it.

Sets can only be built up via computation until they are ultimately used at compile-time for type-checking, after which they are irrelevant.

For example, it is impossible to define any of the following functions:

-}

-- Determines whether A is Bool
-- isBool : Set → Bool
-- isBool A = {!   !}

-- Determines whether A is some type of the form "{...} → {...}"
-- isFunction : Set → Bool
-- isFunction A = {!   !}

-- Determines whether A and B are the same
-- compareSets : Set → Set → Bool
-- compareSets A B = {!   !}

{-

If you try commenting one of them back in, you will see that Agda does not allow you to pattern match on a Set.

Computations like these should more or less be possible at compile-time, and indeed we will learn how to do things like this in a later chapter.

In general though, regular functions don't need to have their types instantiated at compile-time, nor do they pass around runtime type information.

Polymorphic definitions, for instance, are only type-checked once, before they are ever instantiated with specific, concrete types.

The above functions all *should* exist from a mathematician's point of view, and we could define them mathematically in terms of what they *would* do.

They are simply conditions which must always either be true or false, so as mathematical functions we can define them simply by describing them.

However, as *computations* in Agda, they are all undecidable, and thus cannot be defined.

If we really wanted to be able to define these like this as plain Agda functions, Agda would need to sacrifice flexibility in its notion of types.

There are many ways that various languages do this, all with their own compromises, but Agda tries to err on the side of extensibility.

The ability to define something like isBool or compareSets would, for instance, require some restrictive notion of type equality. For example:

data Boolean : Set
  F : Boolean
  T : Boolean

Is this type "equivalent" to Bool? Depending on what we are trying to accomplish, it certainly could be nice if Agda's answer were "yes".

There is no measurable way in which 'Boolean' is different than Bool, so it is going to have all of the same properties.
  
At the same time though, this would require an arbitrary choice to be made by Agda, if compareSets is to be a decideable computation.

Comparing Sets is in fact not the only operation on them which is undecidable:

-}

-- Determines whether there is any element of the type A
-- isNonEmpty : Set → Bool
-- isNonEmpty A = {!   !}

-- Compares two A's (without knowing what exact type A is)
-- genericCompare : {A : Set} → A → A → Bool
-- genericCompare {A} x y = {!   !}

{-

isNonEmpty would require all types to be boring enough that the compiler can trivially determine whether it is possible to construct one.

If Agda can enable us to allow us to define arbitrary mathematical theorems with Sets, then this function clearly couldn't be computable.

Similarly, genericCompare would require all types to somehow have a trivially definable equality comparison function.

This actually sounds reasonable, and many languages have this in some form, though it becomes problematic with fancier types, like function types.

For example, how should we decide whether two functions of type "ℤ → ℤ" are equal for every input? This itself is an undecidable operation.

Additionally, all of these functions would require some form of runtime type information, which can burden a language with a range of complications.

Now, regardless of what restrictions we accept, the fact that we can't define just any mathematically valid function in Agda is quite significant.

We say that programming languages (like Agda) are "constructive" since they don't allow us to define objects or functions which are not computable.

In "classical" mathematics, we may define operations in terms of conditions, regardless of whether these conditions are decidable.

In Agda, on the other hand, we can only define operations by computation, so if we want to use a condition to do this then it must be decidable. 

Purely mathematical functions, which aren't necessarily computable, require a more precise formulation to define in Agda, which will be covered later.

For now though, understanding this helps to some degree to clarify the difference between booleans and propositions in Agda:

Booleans and boolean expressions are always decidable trivially by evaluation, thanks to the restriction that we can only define them by computation.

This means that, when defining a function, we may assume that a Bool is either true or false, and do something different in each case.

Sets, however, interpreted as propositions, are not concrete objects, and are not restricted to having a concrete, computable truth value.

So, when defining a function, it isn't even valid to "assume" that some proposition is either true or false, as this simply is not decidable.

One funny consequence of this is that there even actually are some definitions which literally cannot be proven or disproven:

-}

-- peirces-law : {P Q : Set} → ((P → Q) → P) → P
-- peirces-law f = {!   !}

{-

The above definition is a theorem of classical logic, and, in its raw form, cannot be proven in a constructive proof-assistant like Agda.

It is pretty easy to prove with a truth table, in particular by examining the case where P is false, and the case where P is true:

-}

module _ {True : Set} {False : Set}
         (proof-of-True : True)
         (disproof-of-False : {A : Set} → False → A)
  where

  peirces-law-P=True : {Q : Set} → ((True → Q) → True) → True
  peirces-law-P=True f = {!   !}

  peirces-law-P=False : {Q : Set} → ((False → Q) → False) → False
  peirces-law-P=False f = {!   !}

{-

However, inability to assert that any given proposition is either true or false is exactly what prevents us from proving this more general theorem.

Constructively, if we are to claim that a type is "true", then we need to be able to produce a concrete element of that type.

For better or for worse, this is impossible to do for just any "P", given only a function of type "(P → Q) → P" when we know nothing about "Q".

This function, if it were to exist, would have to magically pull one particular "P" out of thin air, with no information to make this decision with.

Try this out with P = Bool, and notice how there is no general way to decide what particular Bool should be returned.

-}

peirces-law-P=Bool : {Q : Set} → ((Bool → Q) → Bool) → Bool
peirces-law-P=Bool f = {!   !}

{-

As we cover more features of Agda, we will be able to explore these ideas much more precisely, and with more concrete examples.

Although these ideas may be hard to absorb with such abstract examples, it doesn't hurt to try to start building up some intuition for them.

-}

-- practice exercises:

-- note: you are naturally free to write your own definitions (i.e. helper functions)


infixr 4 _=>_ _<=>_

-- Boolean bi-implication, aka Boolean equality
_<=>_ : Bool → Bool → Bool
x <=> y = {!   !}

-- Boolean implication
_=>_ : Bool → Bool → Bool
x => y = {!   !}

-- Boolean not-and
_nand_ : Bool → Bool → Bool
x nand y = {!   !}

-- Define pick for booleans, but only use boolean operators introduced in this exercise (no pattern matching)
pickBool : Bool → Bool → Bool → Bool
pickBool b x y = {!   !}


-- We have defined so many "boolId"s, so it would be nice to have a function to check that these are indeed correct
isId : (Bool → Bool) → Bool
isId f = {!   !}

-- Or even better, simply a function which compares two (Bool → Bool)s for equality
compareBool→Bool : (Bool → Bool) → (Bool → Bool) → Bool
compareBool→Bool f g = {!   !}


-- Here we use new syntactic sugar to declare multiple functions with the same exact type
is-∧ is-∨ is-xor is-=> is-<=> : (Bool → Bool → Bool) → Bool
-- You can fill these in the hard way, or maybe you can write a helper function to make these definitions trivial ...
is-∧ = {!   !}
is-∨ = {!   !}
is-xor = {!   !}
is-=> = {!   !}
is-<=> = {!   !}



-- You may have noticed that pick and unpick demonstrate a bijection between Bool and ({A : Set} → A → A → A)
-- (meaning that both Bool and ({A : Set} → A → A → A) seem to have exactly two distinct values)
-- Lambda terms of this type are known as "Church Encodings" of Bool, and, for performing computations, are equally powerful.
-- Try to define some boolean operations for ({A : Set} → A → A → A) without using pick, unpick, or even Bool

church-not : ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-not f = {!   !}

church-and : ({A : Set} → A → A → A) → ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-and f g = {!   !}

church-or : ({A : Set} → A → A → A) → ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-or f g = {!   !}

church-xor : ({A : Set} → A → A → A) → ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-xor f g = {!   !}

-- We can also use mixfix syntax to define a more familiar notation for "pick"ing operations
infix  0 if_then_else_
if_then_else_ : {A : Set} → Bool → A → A → A
if b then x else y = {!   !}


-- Keep an eye out for opportunities to use higher-order-functions defined in Ch0

combinePredicatesWith : {A : Set} → (Bool → Bool → Bool) → Predicate A → Predicate A → Predicate A
combinePredicatesWith f p q = {!   !}

cmapPredicate : {A B : Set} → (B → A) → Predicate A → Predicate B
cmapPredicate f p = {!   !}

-- We haven't defined any pair or tuple-like type so far, but functions can be very effective at simulating data types
TwoOf : Set → Set
TwoOf A = {!   !} → {!   !}

makeTwoOf : {A : Set} → A → A → TwoOf A
makeTwoOf x y = {!   !}

fst : {A : Set} → TwoOf A → A
fst x = {!   !}

snd : {A : Set} → TwoOf A → A
snd x = {!   !}

swap : {A : Set} → TwoOf A → TwoOf A
swap x = {!   !}

withTwoOf : {A B : Set} → TwoOf A → (A → A → B) → B
withTwoOf x f = {!   !}

mapTwoOf : {A B : Set} → (A → B) → TwoOf A → TwoOf B
mapTwoOf f x = {!   !}

-- A relation aka binary predicate, is a predicate that compares two values of the same type, such as an equality comparison
Relation : Set → Set
Relation A = {!   !}

combineRelationsWith : {A : Set} → (Bool → Bool → Bool) → Relation A → Relation A → Relation A
combineRelationsWith f p q = {!   !}

cmapRelation : {A B : Set} → (B → A) → Relation A → Relation B
cmapRelation f p = {!   !}

flipRelation : {A : Set} → Relation A → Relation A
flipRelation p = {!   !}

runRelationOnTwoOf : {A : Set} → Relation A → TwoOf A → Bool
runRelationOnTwoOf p x = {!   !}

-- Reflexivity means ∀ x, x ~ x
testReflexivityWith : {A : Set} → A → Relation A → Bool
testReflexivityWith x p = {!   !}

-- Symmetry means ∀ x y, x ~ y <=> y ~ x
testSymmetryWith : {A : Set} → A → A → Relation A → Bool
testSymmetryWith x y p = {!   !}

-- Transitivity means ∀ x y z, x ~ y ∧ y ~ z => x ~ z
testTransitivityWith : {A : Set} → A → A → A → Relation A → Bool
testTransitivityWith x y z p = {!   !}

-- Congruence means ∀ x y f, x ~ y => f x ~ f y
testCongruenceWith : {A : Set} → A → A → (A → A) → Relation A → Bool
testCongruenceWith x y f p = {!   !} 

-- We can decently represent a typical "Set<A>" data type as a predicate on A.
-- This is like defining "Set<A>" purely in terms of the behavior of its ".contains" function.
-- However we have already been referring to types as Sets, so it may be unwise to simply refer to "Set<A>" as "a set of A".
-- Rather, a "Set<A>" is technically more like a *subset* of "A", so we should rather think of "Set<A>" as the Set of all subsets of A.
SubsetOf : Set → Set
SubsetOf A = Predicate A

_∈_ : {A : Set} → A → SubsetOf A → Bool
x ∈ a = {!   !}

-- The empty set
∅ : {A : Set} → SubsetOf A
∅ = {!   !}

-- The union of two sets
_∪_ : {A : Set} → SubsetOf A → SubsetOf A → SubsetOf A
a ∪ b = {!   !}

-- The intersection of two sets
_∩_ : {A : Set} → SubsetOf A → SubsetOf A → SubsetOf A
a ∩ b = {!   !}

-- The difference of one set from another
_∖_ : {A : Set} → SubsetOf A → SubsetOf A → SubsetOf A
a ∖ b = {!   !}

-- There are quite a few operations on SubsetOf that we can't seem to define, such as insertion or equality comparison.
-- This is because these operations are not computationally decideable without more information about the specific "A" we are working with.
-- Insertion requires a way to compare two "A"s for equality, which we cant do without knowing what type that A is.
-- Equality comparison of two "Subset A"s is even worse, as it requires us to be able to finitely enumerate every A.
-- Eventually, we will be able to prove the mere existence of these functions, but they cannot be defined constructively in Agda.
-- (Though they can of course be defined if they require A to have the necessary properties, which we will soon learn how to do)


{-

open-ended exercises:

Prove some properties about your definitions, such as:
  ∀ x, x ≡ not (not x)
  ∀ x, x ≡ x ∧ x
  ∀ x y, x ∨ y ≡ y ∨ x
  ∀ x y, x ≡ x ∨ (x ∧ y)
  ∀ x y, not (x ∨ y) ≡ not y ∧ not x
  ∀ x y z, x ∨ (y ∨ z) ≡ (x ∨ y) ∨ z
  ∀ x y z, x ∧ (y ∨ z) ≡ (x ∧ y) ∨ (x ∧ z)

Define a type "Trool" with constructors "yes", "no" and "maybe".
  Define some operations for performing "trinary logic".
  What kinds of laws and properties do these operations have?
  How do they compare to their corresponding Boolean operations?

Define a type "QuadrantalAngle" with constructors "0-deg", "90-deg", "180-deg", and "270-deg".
  Define some operations on this type.
  Prove some laws about these operations.

-}


-- challenge exercises:


-- A Boolean function based encoding of a type with 2^32 distinct values
Uint32 : Set
Uint32 = Bool → Bool → Bool → Bool → Bool → Bool

zero one two three four five six seven : Uint32

zero a b c d e = {!   !}

one a b c d e = {!   !}

two a b c d e = {!   !}

three a b c d e = {!   !}

four a b c d e = {!   !}

five a b c d e = {!   !}

six a b c d e = {!   !}

seven a b c d e = {!   !}

-- hint:
-- At this point, we don't have access to any kind of recursion yet, so our definitions need to contain an easy-to-write number of steps.
-- Define operations on this type using 5 steps of boilerplate/recursion rather than with 2^5 or 2^2^5 steps of boilerplate/recursion.

isZero : Uint32 → Bool
isZero = {!   !}

equals : Uint32 → Uint32 → Bool
equals = {!   !}

increment : Uint32 → Uint32
increment = {!   !}

applyNTimes : {A : Set} → Uint32 → (A → A) → A → A
applyNTimes = {!   !}


-- If we assume one non-constructive law, like peirces law, we can prove a bunch of other non-constructive laws.
-- Once we can pull values out of thin air, types lose their computational meaning to some degree.
-- This lets us define anything which makes sense as a proposition, regardless of its computational meaning.
module ClassicalLogic
  {False : Set}
  (disproof-of-False : {A : Set} → False → A)
  (peirces-law : {P Q : Set} → ((P → Q) → P) → P)
  where

  infixr 5 _Or_

  _Or_ : Set → Set → Set
  A Or B = (A → B) → B

  sym-Or : {A B : Set} → A Or B → B Or A
  sym-Or {A} {B} = {!   !}

  Not : Set → Set
  Not A = A → False

  lem : {A : Set} → (Not A) Or A
  lem {A} = {!   !}

  dne : {A : Set} → Not (Not A) → A
  dne {A} = {!   !}

  infixr 6 _And_

  _And_ : Set → Set → Set
  A And B = Not (A → Not B)

  left : {A B : Set} → A And B → A
  left {A} {B} = {!   !}

  right : {A B : Set} → A And B → B
  right {A} {B} = {!   !}

  demorgan : {A B : Set} → Not (Not A And Not B) → A Or B
  demorgan {A} {B} = {!   !}