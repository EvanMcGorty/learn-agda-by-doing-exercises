{-# OPTIONS --safe --cubical-compatible #-}

{-

Welcome to the next chapter.

This file will define an actual concrete data type, the booleans, and explore what kinds of functions we can write with them.

We will also learn how to define aliases for types and write some basic type level functions.

Once again, we will start out with a module declaration.

However this time, we will also import everything from the previous chapter.

-}

module Solutions.Ch1 where


open import Solutions.Ch0

{-

"open import XYZ" is syntactic sugar for "import XYZ" followed by "open XYZ"

Importing a file brings its module into scope, so we can access its definitions with "MyModuleName.myDefinition"

"open"-ing it simply allows us to refer to symbols without prefixing them like this, i.e. simply "myDefinition"

The "open"-ness of a module is local (by default). So if someone opens Ch1, Ch0 would not be open to them (though it would be in scope).

Now get ready for a definition:

-}


data Bool : Set where
  false : Bool
  true : Bool
-- In Agda we use whitespace instead of tabs, and we generally indent in increments of two spaces
-- Your editor should be smart enough to handle this automatically when you press tab

{-

This definition declares a data type, "Bool", which is a Set (i.e. a type).

Furthurmore it declares that there is a Bool named "false", and a Bool named "true".

false and true are called constructors, as they are ways to construct a Bool.

In Agda, no data types are automatically baked directly into the language, so we either have to define to define them by hand or use a library.

So, this is kind of like manually writing the following definition in a more traditional programming langauge:

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
idBool0 x = x

-- Defined in terms of idBool0
idBool1 : Bool → Bool
idBool1 x = idBool0 x

-- Simply equal to idBool0 (thanks to eta-equivalence)
idBool2 : Bool → Bool
idBool2 = idBool0

-- Defined in terms of id, with an explicit type parameter
idBool3 : Bool → Bool
idBool3 x = id {Bool} x

-- Simply equal to id, with an explicit type parameter
idBool5 : Bool → Bool
idBool5 = id {Bool}

-- Defined in terms of id, but taking advantage of type inference
idBool4 : Bool → Bool
idBool4 x = id x

-- Simply equal to id, but taking advantage of type inference
idBool6 : Bool → Bool
idBool6 = id

{-

It is worth noting that these identity functions aren't really meaningful propositions/proofs, as Bool itself isn't a meaningful proposition.

Bool is just a data type, or a Set with two elements. It is trivial to construct a Bool, which is what it would mean to "proove Bool".

So, if anything, since it is a nonempty Set, Bool is a trivially true proposition. But it is really just a datatype.

When we eventually formalize our notion of logical propositions in Agda, we will see that they at least share many similarities with Bool.

For now though, "true" and "false" are just arbitrary names, and don't immediately have anything to do with propositional truth or falsity in Agda.

Moving on, lets define some more interesting operations on booleans:

-}

not : Bool → Bool
not false = true
not true = false

{-

In order to define this function, we need to perform case analysis, aka pattern matching.

This can be done by hand, but instead we will use a new interactive command, called case split.

To case split, type an argument which you wish to inspect into the hole, and press C-c C-c.

This allows you to define a result for each constructor defined in the data declaration of the data type.

Note: at this point, C-c C-f and C-c C-b to move back and forth between holes become very helpful, remember to use them

Funnily enough, this even gives us another way to define the identity function:

-}

idBool7 : Bool → Bool
idBool7 false = false
idBool7 true = true

{-

In fact, there are many more ways to do this.

When we pattern match, we have to cover all cases. But, one way to cover all cases is to simply use a variable:

-}

idBool8 : Bool → Bool
idBool8 false = false
idBool8 x = true

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
idBool9 x = x
-- idBool9 true = false
-- commenting the above line back in yields a warning.

{-

It can however still be very useful to write code without overlapping cases, which can make it easier to reason about the correctness of code.

Another useful interactive command is normalize (a fancy word for "evaluate"), which lets you actually run code.

Simply type C-c C-n (which opens an interactive window) and then type in an expression which you wish to normalize.

If you have a hole available, you can also type the expression into it, and then press C-c C-n.

Please use this to test your code.

Now, let's move on to some more interesting definitions:

-}

-- Don't worry to much about this, we are just declaring the precedence of our newly defined operators
infixr 6 _∧_ _&&_
infixr 5 _∨_ _||_ _xor_

-- read as "and", typed as \and
_∧_ : Bool → Bool → Bool
false ∧ y = false
true ∧ y = y

-- read as "or", typed as \or
_∨_ : Bool → Bool → Bool
false ∨ y = y
true ∨ y = true

_xor_ : Bool → Bool → Bool
false xor y = y
true xor y = not y

-- Here are some ascii synonyms, as promised :^)
-- (for such simple definitions with no variables, we don't need an explicit type signature)
_&&_ = _∧_
_||_ = _∨_


{-

Here you probably immediately notice the strange use of underscores. Names with underscores are able to be used with operator-like syntax.

For example, if we defined _foo_bar_baz_, not only can we write "_foo_bar_baz_ a b c d", but we can also write "a foo b bar c baz d".

This feature of Agda, called "mixfix", is of course more than enough to define some simple infix operators, as we have done above.

We also declared these operators to associate to the right, so "x ∧ y ∧ z" is understood as "x ∧ (y ∧ z)".

We also gave "∧" a higher precedence than "∨", so "w ∨ x ∧ y ∨ z" is understood as "w ∨ (x ∧ y) ∨ z".

Take your time to define the above functions, and experiment with different ways of doing so (i.e. case splitting once vs twice).

We can also use Bool to define new polymorphic functions:

-}

-- "pick false" should return the first supplied "A", "pick true" should return the second suplied "A"
pick : Bool → {A : Set} → A → A → A
pick false = λ x y → x
pick true = λ x y → y

-- should be the inverse of pick
unpick : ({A : Set} → A → A → A) → Bool
unpick f = f false true


{-

Notice how we can introduce a generic parameter anywhere we want, i.e. for any Bool, pick returns a generic function.

Likewise, unpick *accepts* a generic function but itself is actually not generic.

unpick will only accept an argument which works for *any* Set, so you cannot supply it a Bool → Bool → Bool, for instance.

Conversely, this means that (the definition of) unpick has the freedom to use this argument with whatever type it wants.

For the last topic of this chapter, we will take a peek at some type-level programming.

In Agda, types are actually just values and can consequently be used wherever you might normally use values.

This means that we can even define functions that return Sets, and use them in type signatures.

The Symbol "Set" is not only used to quantify over generic type-parameters, but is actually itself just another Type, like "Bool".

This means that we can write a function which returns a Set, and can use it in a type expression:

-}

MyDependentType : Bool → Set
MyDependentType false = Bool → Bool
MyDependentType true = Bool → Bool → Bool

not2 : MyDependentType false
not2 = not

_xor2_ : MyDependentType true
_xor2_ = _xor_

idBool10 : MyDependentType (false ∨ false ∧ true)
idBool10 = id

{-

This effectively lets us run arbitrary code during typechecking, which is pretty cool.

In later chapters, we will use this to write some really powerful type-level code.

Even for now, this gives us a very simple and nice way of defining aliases for types:

-}

Bit : Set
Bit = Bool

0bit : Bit
0bit = false

1bit : Bit
1bit = true

invertBit : Bit → Bit
invertBit = not

-- Not only can we return types, but we can also take them as arguments
Predicate : Set → Set
Predicate A = A → Bool

invertPredicate : {A : Set} → Predicate A → Predicate A
invertPredicate p = λ x → not (p x)

andPredicate : {A B : Set} → Predicate A → Predicate A → Predicate A
andPredicate p q = λ x → p x ∧ q x

{-

Working with aliases is a great example of where interactive editing in Agda shines.

Aliases can be confusing to work with, and Agda provides three variants for displaying type information:
  1. C-c: This is the default variant, which you have used until now
  2. C-y: This specifically evaluates/simplifies all types in a context as much as possible before displaying them
  3. C-u: This specifically avoids as much evaluation/simplification as possible when displaying types

So, if you inspect the context of a hole with C-c C-, and "Predicate A" is confusing, using C-y C-, instead will display this as A → Bool

Alternatively, if something is ever getting simplified to a point that makes it harder to read, C-u C-, might be helpful.

This works for the majority of commands that involve displaying types, not just C-c C-,

Moving on, here are some exercises to practice the above content:

-}

-- note: you are naturally free to write your own defintions (i.e. helper functions)

infixr 4 _=>_ _<=>_

-- Boolean bi-implication, aka Boolean equality
_<=>_ : Bool → Bool → Bool
false <=> y = not y
true <=> y = y

-- Boolean implication
_=>_ : Bool → Bool → Bool
x => y = not x ∨ y

-- Boolean not-and
_nand_ : Bool → Bool → Bool
x nand y = not (x ∧ y)

-- Define pick for booleans, but only use boolean operators introduced in this exercise (no pattern matching)
pickBool : Bool → Bool → Bool → Bool
pickBool b x y = (not b => x) ∧ (b => y)


-- We have defined so many "boolId"s, so it would be nice to have a function to check that these are indeed correct
isId : (Bool → Bool) → Bool
isId f = f true ∧ not (f false)

-- Or even better, simply a function which compares two (Bool → Bool)s for equality
compareBool→Bool : (Bool → Bool) → (Bool → Bool) → Bool
compareBool→Bool f g = (f false <=> g false) ∧ (f true <=> g true)


compareBool→Bool→Bool : (Bool → Bool → Bool) → (Bool → Bool → Bool) → Bool
compareBool→Bool→Bool f g = compareBool→Bool (f false) (g false) ∧ compareBool→Bool (f true) (g true)

-- Here we use new syntactic sugar to declare multiple functions with the same exact type
is-∧ is-∨ is-xor is-=> is-<=> : (Bool → Bool → Bool) → Bool
-- You can fill these in the hard way, or maybe you can write a helper function to make these definitions trivial ...
is-∧ = compareBool→Bool→Bool _∧_
is-∨ = compareBool→Bool→Bool _∨_
is-xor = compareBool→Bool→Bool _xor_
is-=> = compareBool→Bool→Bool _=>_
is-<=> = compareBool→Bool→Bool _<=>_



-- You may have noticed that pick and unpick demonstrate a bijection between Bool and ({A : Set} → A → A → A)
-- (meaning that both Bool and ({A : Set} → A → A → A) seem to have exactly two distinct values)
-- Lambda terms of this type are known as "Church Encodings" of Bool, and, for performing computations, are equally powerful.
-- Try to define some boolean operations for ({A : Set} → A → A → A) without using pick, unpick, or even Bool

church-not : ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-not f = λ x y → f y x

church-and : ({A : Set} → A → A → A) → ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-and f g = λ x y → f x (g x y)

church-or : ({A : Set} → A → A → A) → ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-or f g = λ x y → f y (g y x)

church-xor : ({A : Set} → A → A → A) → ({A : Set} → A → A → A) → {A : Set} → A → A → A
church-xor f g = λ x y → f (g x y) (g y x)

-- We can also use mixfix syntax to define a more familiar notation for "pick"ing operations
infix  0 if_then_else_
if_then_else_ : {A : Set} → Bool → A → A → A
if false then x else y = y
if true then x else y = x


-- Keep an eye out for oportunities to use higher-order-functions defined in Ch0

combinePredicatesWith : {A : Set} → (Bool → Bool → Bool) → Predicate A → Predicate A → Predicate A
combinePredicatesWith f p q = λ x → f (p x) (q x) -- combinePredicatesWith = lift

cmapPredicate : {A B : Set} → (B → A) → Predicate A → Predicate B
cmapPredicate f p = λ x → p (f x) -- cmapPredicate = flip compose

-- We haven't defined any pair or tuple-like type so far, but functions can be very effective at simulating datatypes
TwoOf : Set → Set
TwoOf A = Bool → A

makeTwoOf : {A : Set} → A → A → TwoOf A
makeTwoOf x y = λ b → pick b x y

fst : {A : Set} → TwoOf A → A
fst x = x false -- fst = flip apply false

snd : {A : Set} → TwoOf A → A
snd x = x true -- snd = flip apply true

swap : {A : Set} → TwoOf A → TwoOf A
swap x = λ b → pick b (x true) (x false)

withTwoOf : {A B : Set} → TwoOf A → (A → A → B) → B
withTwoOf x f = f (x false) (x true)

mapTwoOf : {A B : Set} → (A → B) → TwoOf A → TwoOf B
mapTwoOf f x = λ b → f (x b) -- mapTwoOf = compose

-- A relation aka binary predicate, is a predicate that compares two values of the same type, such as an equality comparison
Relation : Set → Set
Relation A = A → A → Bool

combineRelationsWith : {A : Set} → (Bool → Bool → Bool) → Relation A → Relation A → Relation A
combineRelationsWith f p q = λ x y → f (p x y) (q x y)

cmapRelation : {A B : Set} → (B → A) → Relation A → Relation B
cmapRelation f p = λ x y → p (f x) (f y) -- cmapRelation = flip on

flipRelation : {A : Set} → Relation A → Relation A
flipRelation p = λ x y → p y x -- flipRelation = flip

runRelationOnTwoOf : {A : Set} → Relation A → TwoOf A → Bool
runRelationOnTwoOf p x = p (x false) (x true) -- runRelationOnTwoOf = flip withTwoOf

-- Reflexivity means ∀ x, x ~ x
testReflexivityWith : {A : Set} → A → Relation A → Bool
testReflexivityWith x p = p x x

-- Symmetry means ∀ x y, x ~ y <=> y ~ x
testSymmetryWith : {A : Set} → A → A → Relation A → Bool
testSymmetryWith x y p = p x y <=> p y x

-- Transitivity means ∀ x y z, x ~ y ∧ y ~ z => x ~ z
testTransitivityWith : {A : Set} → A → A → A → Relation A → Bool
testTransitivityWith x y z p = p x y ∧ p y z <=> p x z

-- Congruence means ∀ x y f, x ~ y => f x ~ f y
testCongruenceWith : {A : Set} → A → A → (A → A) → Relation A → Bool
testCongruenceWith x y f p = p x y => p (f x) (f y) 

-- We can decently represent a typical "Set<A>" data type as a predicate on A
-- This is like defining "Set<A>" purely in terms of the behavior of its ".contains" function
-- However we have already been referring to Types as Sets, so it may be unwise to simply refer to "Set<A>" as "a set of A".
-- Rather, a "Set<A>" is technically more like a *subset* of "A", so we should rather think of "Set<A>" as the Set of all subsets of A.
SubsetOf : Set → Set
SubsetOf A = Predicate A

_∈_ : {A : Set} → A → SubsetOf A → Bool
x ∈ a = a x

-- The empty set
∅ : {A : Set} → SubsetOf A
∅ = λ x → false

-- The union of two sets
_∪_ : {A : Set} → SubsetOf A → SubsetOf A → SubsetOf A
a ∪ b = λ x → a x ∨ b x

-- The intersection of two sets
_∩_ : {A : Set} → SubsetOf A → SubsetOf A → SubsetOf A
a ∩ b = λ x → a x ∧ b x

-- The difference of one set from another
_∖_ : {A : Set} → SubsetOf A → SubsetOf A → SubsetOf A
a ∖ b = λ x → a x ∧ not (b x)

-- There are quite a few operations on SubsetOf that we can't seem to define, such as insertion or equality comparison.
-- This is because these operations are not computationally decideable without more information about the specific "A" we are working with.
-- Insertion requires a way to compare two "A"s for equality, which we cant do without knowing what type that A is.
-- Equality comparison of two "Subset A"s is even worse, as it requires us to be able to finitely enumerate every A.
-- Eventually, we will be able to prove the mere existence of these functions, but they cannot be defined constructively in Agda.
-- (Though they can of course be defined if they require A to have the necessary properties, which we will soon learn how to do)


{-

open-ended exercises:

Define a type "Trool" with constructors "yes", "no" and "maybe".
  Define some operations for performing "trinary logic".
  What kinds of laws and properties do these operations have?
  How do they compare to their corresponding Boolean operations?

Define a type "QuadrantalAngle" with constructors "0-deg", "90-deg", "180-deg", and "270-deg".
  Define some operations on this type.
  Prove some laws about these operations.

-}


-- challenges:


-- A Boolean function based encoding of a type with 2^32 distinct values
Uint32 : Set
Uint32 = Bool → Bool → Bool → Bool → Bool → Bool

zero one two three four five six seven : Uint32

zero a b c d e = false

one false false false false false = true
one a b c d e = false

two false false false false true = true
two a b c d e = false

three false false false false false = true
three false false false false true = true
three a b c d e = false

four false false false true false = true
four a b c d e = false

five false false false false false = true
five false false false true false = true
five a b c d e = false

six false false false false true = true
six false false false true false = true
six a b c d e = false

seven false false false false false = true
seven false false false false true = true
seven false false false true false = true
seven a b c d e = false

-- hint: At this point, we don't have access to any kind of recursion yet, so our definitions need to contain an easy-to-write number of steps.
--       Define operations on this type using 5 steps of boilerplate/recursion rather than with 2^5 or 2^2^5 steps of boilerplate/recursion.

isZeroLift : {A : Set} → (A → Bool) → ((Bool → A) → Bool)
isZeroLift isZero' x = isZero' (x false) ∧ isZero' (x true)

isZero1 = not
isZero2 = isZeroLift isZero1
isZero4 = isZeroLift isZero2
isZero8 = isZeroLift isZero4
isZero16 = isZeroLift isZero8

isZero : Uint32 → Bool
isZero = isZeroLift isZero16

equalsLift : {A : Set} → (A → A → Bool) → ((Bool → A) → (Bool → A) → Bool)
equalsLift equals' x y = equals' (x false) (y false) ∧ equals' (x true) (y true)

equals1 = _<=>_
equals2 = equalsLift equals1
equals4 = equalsLift equals2
equals8 = equalsLift equals4
equals16 = equalsLift equals8

equals : Uint32 → Uint32 → Bool
equals = equalsLift equals16

succLift : {A : Set} → (A → Bool) → (A → A) → (Bool → A) → (Bool → A)
succLift isZero' succ' x false = succ' (x false)
succLift isZero' succ' x true = if isZero' (succ' (x false)) then succ' (x true) else x true

succ1 = not
succ2 = succLift isZero1 succ1
succ4 = succLift isZero2 succ2
succ8 = succLift isZero4 succ4
succ16 = succLift isZero8 succ8

succ : Uint32 → Uint32
succ = succLift isZero16 succ16

maxLift : {A : Set} → A → (Bool → A)
maxLift x b = x

max1 = true
max2 = maxLift max1
max4 = maxLift max2
max8 = maxLift max4
max16 = maxLift max8

applyNTimesLift : {A : Set} → A → ({B : Set} → A → (B → B) → B → B) → ({B : Set} → (Bool → A) → (B → B) → B → B)
applyNTimesLift max' applyNTimes' n f x = applyNTimes' (n true) (λ y → applyNTimes' max' f (f y)) (applyNTimes' (n false) f x)

applyNTimes1 : {A : Set} → Bool → (A → A) → A → A
applyNTimes1 false f x = x
applyNTimes1 true f x = f x

applyNTimes2 = applyNTimesLift max1 applyNTimes1
applyNTimes4 = applyNTimesLift max2 applyNTimes2
applyNTimes8 = applyNTimesLift max4 applyNTimes4
applyNTimes16 = applyNTimesLift max8 applyNTimes8

applyNTimes : {A : Set} → Uint32 → (A → A) → A → A
applyNTimes = applyNTimesLift max16 applyNTimes16