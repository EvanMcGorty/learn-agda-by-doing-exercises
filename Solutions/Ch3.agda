{-# OPTIONS --safe --cubical-compatible #-}

module Solutions.Ch3 where

{-

In this chapter we will explore so called "products" in Agda, and find out why such a familiar programming concept is given such a strange name.

We will also look at the laws bijective functions, and define some bijections as well as some type-generic operations on bijections.

Once again, take a look at the imports for this chapter:

-}

open import Imports.Ch3

{-

Products are simply data types which combine a few other data types into one, like a struct or a tuple in a more typical language.

Lets look at a simple example:

-}

data BoolAndNat : Set where
  makeBoolAndNat : Bool → ℕ → BoolAndNat

-- Equivalent ADT syntax:
-- data BoolAndNat = makeBoolAndNat Bool Nat

{-

Here we have defined a simple pair (or "product") of Bool and ℕ.

At first this may seem like a pretty cryptic way of doing so, but it can be understood as follows:

  BoolAndNat is a Set, and there is exactly one way to construct a BoolAndNat, which we will call makeBoolAndNat.

  makeBoolAndNat takes a Bool, and then a ℕ, and results in a BoolAndNat.

In a more traditional language, this might look like the following:

class BoolAndNat {
  Bool bool;
  Nat nat;
}

This looks quite a bit different, and actually much more natural. It declares that every BoolAndNat contains a Bool and a ℕ.

From this, it's pretty obvious that a BoolAndNat can be constructed given a Bool and a ℕ.

The Agda code, on the other hand, declares that in order to make a BoolAndNat, you must supply a Bool and ℕ.

From this we may infer that BoolAndNat also contains a Bool and a ℕ, which is understandable, but still a bit strange.

Lets start out by constructing a few BoolAndNats:

-}

falseAndSix : BoolAndNat
falseAndSix = makeBoolAndNat false 6

trueAndEleven : BoolAndNat
trueAndEleven = makeBoolAndNat true 11

boolWithBitValue : Bool → BoolAndNat
boolWithBitValue false = makeBoolAndNat false 0
boolWithBitValue true = makeBoolAndNat true 1

natWithPositivity : ℕ → BoolAndNat
natWithPositivity zero = makeBoolAndNat false zero
natWithPositivity (suc n) = makeBoolAndNat true (suc n)

{-

So, how do we access the Bool or the ℕ? With pattern matching.

This data type only has one case, but pattern matching allows us to access the arguments passed to the constructor, just like the "suc" case of ℕ.

Try it out, as per usual with C-c C-c:

-}

getBool : BoolAndNat → Bool
getBool (makeBoolAndNat b n) = b

getNat : BoolAndNat → ℕ
getNat (makeBoolAndNat b n) = n

{-

One the one hand, this is a bit annoying. In most languages we could just write x.bool or x.nat, but in Agda we need to define functions which do this.

On the other hand, however, if we reorient ourselves to usually just use pattern matching, this doesn't actually matter as much.

In fact, definitions written with pattern matching can be subjectively more readable, as they make for more readable and direct equations.

Try experimenting with the difference between using pattern matching and using the accessors which we have just defined.

-}

invertBool : BoolAndNat → BoolAndNat
invertBool (makeBoolAndNat b n) = makeBoolAndNat (not b) n

incrementNat :  BoolAndNat → BoolAndNat
incrementNat (makeBoolAndNat b n) = makeBoolAndNat b (suc n)

swapWith : (ℕ → Bool) → (Bool → ℕ) → BoolAndNat → BoolAndNat
swapWith f g (makeBoolAndNat b n) = makeBoolAndNat (f n) (g b)

{-

Are you getting tired of typing in makeBoolAndNat every time you want to create a BoolAndNat? Well, interactive editing is here to save the day.

While inside of a hole where the "Goal" type only has one constructor, like BoolAndNat does, you can use C-c C-r, "refine", to fill this in.

This is a variant on one of the modes of "refine" discussed in Ch0, except that the constructor is implied, so we don't have to type it in manually.

Of course you could type your desired function in and then press C-c C-r, but if the hole is blank, then the constructor is inferred.

-}

makeBoolAndNatWith : {A : Set} → (A → Bool) → (A → ℕ) → A → BoolAndNat
makeBoolAndNatWith f g x = makeBoolAndNat (f x) (g x)

withBoolAndNat : {A : Set} → (BoolAndNat → A) → Bool → ℕ → A
withBoolAndNat f x n = f (makeBoolAndNat x n)

updateNat : (ℕ → ℕ) → BoolAndNat → BoolAndNat
updateNat f (makeBoolAndNat b n) = makeBoolAndNat b (f n)

compare-BoolAndNat : BoolAndNat → BoolAndNat → Bool
compare-BoolAndNat (makeBoolAndNat x n) (makeBoolAndNat y m) = (x <=> y) ∧ (n ≡ᵇ m)

{-

When we have a BoolAndNats as an argument, we can pattern match on it to access its fields, so we don't have to use accessor functions.

However when we get a BoolAndNat back from another function, it would seem that we are forced to use an accessor function.

This is fine when we only need one field, but if we need multiple then it can cause a bit of duplication:

-}

extractBool : {A : Set} → (A → BoolAndNat) → A → Bool
extractBool f x = getBool (f x)

mapBoolAndNat : {A : Set} → (A → BoolAndNat) → (Bool → Bool) → (ℕ → ℕ) → A → BoolAndNat
mapBoolAndNat f g h x = makeBoolAndNat (g (getBool (f x))) (h (getNat (f x)))

composeWithBoolAndNat : {A B : Set} → (A → BoolAndNat) → (Bool → ℕ → B) → A → B
composeWithBoolAndNat f g x = g (getBool (f x)) (getNat (f x))

{-

This isn't so bad in this case, but raises a question: how do we avoid duplicate expressions?

One solution would be to use some of the higher order functions which we defined in Ch0, but this is still a bit unsatisfying.

A more natural solution is to use 'let' and 'where' bindings to define a local variable, and assign the duplicated expression to that.

For example, the following definition:

foo n = (2 + n) * (2 + n)

can be simplified to use 'let' like:

foo n = let m = 2 + n in m * m

or can be simplified to use where like:

foo n = m * m where m = 2 + n

In fact, let and where bindings are readable, but technically we only need lambdas to accomplish the same thing:

foo n = (λ m → m * m) (2 + n)

However, let and where bindings can be much more readable, if the user is smart with newlines and indentation:

foo n = let m = 2 + n
         in m * m
         
foo n = m * m
  where
    m = 2 + n

Indentation and newlines in Agda are very flexible, and once you get a feel for them, give you lots of freedom to write readable code.

Let and where bindings can even be functions, provided we give them a type signature:

foo n = f 2 * f 3
  where
    f : ℕ → ℕ -- If you want to use a type variable here, be sure to bring it into scope
    f x = x + n

'where' and 'let' also permit multiple definitions. Feel free to check out the official Agda documentation for more.

Try out all of the above mentioned tricks, and be sure to practice using let and where bindings throughout the rest of the exercises as necessary.

Notes:
  'let' bindings are just plain expressions which can be used anywhere in any expression.
  'where' on the other hand is bound to a single definition (i.e. a single "=") and can't simply be placed anywhere in an expression.
  'let' bindings are weaker than 'where' bindings and global declarations, as they don't allow case analysis or recursion.

-}

composeBoolsAndNats : {A : Set} → (BoolAndNat → A) → (A → Bool) → (A → ℕ) → BoolAndNat → BoolAndNat
composeBoolsAndNats f g h x =
  let y = f x
   in makeBoolAndNat (g y) (h y)

doubleComposeBoolAndNat : {A B : Set} → (A → BoolAndNat) → (BoolAndNat → BoolAndNat → B) → A → B
doubleComposeBoolAndNat f g x = g y y
  where
    y = f x

compare-Bool-to-BoolAndNat : (Bool → BoolAndNat) → (Bool → BoolAndNat) → Bool
compare-Bool-to-BoolAndNat f g = compare-on false ∧ compare-on true
  where
    compare-on : Bool → Bool
    compare-on x = compare-BoolAndNat (f x) (g x)

{-

In Agda, there is another, more idiomatic way of defining such "products", which is much more reminiscent of traditional programming languages.

These are called records, and are basically just a "struct", with named fields as one would expect.

This time we will define a type which contains three Bools, instead of one Bool and one Nat:

-}

record ThreeBools : Set where
  field
    fst : Bool
    snd : Bool
    thd : Bool

{-

This definition is arguably more readable than the equivalent definition as a data type:

data ThreeBools : Set where
  makeThreeBools : Bool → Bool → Bool → ThreeBools

And reads more like its equivalent in a more typical language:

class ThreeBools {
  Bool fst;
  Bool snd;
  Bool thd;
}

It also has the advantage that it gives you accessor functions for free.

The only thing missing here is a constructor function, which is replaced with the following syntax:

-}

false-false-false : ThreeBools
false-false-false = record { fst = false ; snd = false ; thd = false }

{-

This may look annoying to type in, but as with BoolAndNat, we can just use C-c C-r, and Agda will fill this in for us automatically.

-}

true-true-true : ThreeBools
true-true-true = record { fst = true ; snd = true ; thd = true }

{-

Additionally, pattern matching (automatically with C-c C-c) also uses this record syntax:

-}

is-false-true-true : ThreeBools → Bool
is-false-true-true record { fst = false ; snd = true ; thd = true } = true
is-false-true-true record { fst = fst ; snd = snd ; thd = thd } = false

{-

However if you still don't like this, we can also declare a record to come pre-packaged with a constructor:

-}

record TwoNats : Set where
  constructor makeTwoNats
  field
    fst : ℕ
    snd : ℕ

{-

And although you can still write out record syntax, C-c C-c and C-c C-r will now default to using this constructor.

-}

zero-zero : TwoNats
zero-zero = makeTwoNats 0 0

is-one-one : TwoNats → Bool
is-one-one (makeTwoNats 1 1) = true
is-one-one (makeTwoNats fst snd) = false


{-

In most languages, fields of a data type are not global functions, and are usually accessed through dot-syntax (i.e. "foo.bar")

This means that these names do not pollute the global namespace, and different data types can reuse the same names for their fields.

In Agda however, we don't have dot-syntax for members of data types, and accessors are just regular functions.

So to prevent ambiguity, records come with a module of the same name, which contains their accessors.

We can use these accessors after opening these modules just like we have opened other modules until now, i.e.

  open ThreeBools

  first-of-myThreeBools = fst myThreeBools

Though in this case, it might be more reasonable to just directly access them through their respective modules

  first-of-myThreeBools = ThreeBools.fst myThreeBools

This avoids polluting the global namespace, and helps prevent ambiguities.

Agda's module system is very powerful, in fact quite a bit more powerful than you need for a tutorial like this.

Nonetheless, I encourage you to try reading the official documentation on records and modules if you ever feel the need to.

Despite now having automatically generated accessors though, pattern matching is still completely viable and often leads to more readable code.

Remember to experiment with both in various scenarios and see when each one is better (subjectively).

-}

get-first-elements : ThreeBools → TwoNats → BoolAndNat
get-first-elements x y = makeBoolAndNat (ThreeBools.fst x) (TwoNats.fst y)

addTwoNats : TwoNats → ℕ
addTwoNats x = TwoNats.fst x + TwoNats.snd x

invertThreeBools : ThreeBools → ThreeBools
invertThreeBools x = record { fst = not (ThreeBools.fst x) ; snd = not (ThreeBools.snd x) ; thd = not (ThreeBools.thd x) }

{-

In Agda, we can even define a generalized form of such data types, as we can in most modern languages with generics.

This lets us write generic operations which work over products of any data type:

-}

record Pair (A : Set) (B : Set) : Set where
  constructor makePair
  field
    fst : A
    snd : B

-- Bring fst and snd into public scope, so you don't need to write out Pair.fst and Pair.snd
open Pair

seven-and-false : Pair ℕ Bool
seven-and-false = makePair 7 false

swap : {A B : Set} → Pair A B → Pair B A
swap (makePair x y) = makePair y x

reorderPairs : {A B C : Set} → Pair A (Pair B C) → Pair (Pair A B) C
reorderPairs (makePair x (makePair y z)) = makePair (makePair x y) z

{-

This definition of Pair is pretty reasonable, and resembles mainstream languages quite a bit, i.e.

class Pair<A, B> {
  A fst;
  B snd;
}

The first line of the declaration of Pair can be read as:

  For any two Sets, "A" and "B", "Pair A B" is a Set

Notably, however, is the use of parentheses for the type parameters here, instead of curly braces like we normally use with polymorphic functions.

Most languages handle type parameters to generic data types separately from how they handle normal parameters to normal functions, unlike here.

Here, Pair has normal parameters like a "Set → Set → Set", so we write parameters like "Pair ℕ Bool" rather than "Pair {ℕ} {Bool}"

In fact, Pair *is* a "Set → Set → Set":

-}

My-Set→Set→Set : Set → Set → Set
My-Set→Set→Set = Pair

{-

Pair is called a type constructor (analogously to data constructors), because unlike most functions, when you apply its arguments, it doesn't reduce.

Instead, its saturated form (i.e. with arguments applied) represents a complete, fully reduced type.

The way that Agda so casually treats types just like values can be a bit mind bending at first, so take your time to wrap your head around this.

Most languages have some kind of syntactical barrier between types and values, making it easier to keep track of what is a type and what is a value.

Agda on the other hand embraces their commonalities. For example, we can also naturally write the following:

-}

-- Notice how, despite the lack of a syntactic barrier between types and values, we still try to write types in uppercase for readability
My-Set→Set→Set' : Set → Set → Set
My-Set→Set→Set' A B = Pair A B

{-

And of course it doesn't stop there. We can define all sorts of type synonyms, just like we have done in previous chapters:

-}

FlippedPair : Set → Set → Set
FlippedPair A B = Pair B A

PairOfNatAnd : Set → Set
PairOfNatAnd A = Pair ℕ A

TwoOf : Set → Set
TwoOf A = Pair A A

seven-and-false2 : PairOfNatAnd Bool
seven-and-false2 = seven-and-false

swap2 : {A B : Set} → Pair A B → FlippedPair A B
swap2 = swap

ApplyToNat : (Set → Set) → Set
ApplyToNat F = F ℕ

-- This one might be a bit tricky :^)
runOnTwoOfNat : {A : Set} → (ℕ → ℕ → A) → ApplyToNat TwoOf → A
runOnTwoOfNat = λ f x → f (fst x) (snd x)

{-

All of these functions are just as usable as you would hope them to be, because simply reduce during typechecking.

Another really cool thing about Pair, is that it has a meaningful logical interpretation.

Bool and ℕ are trivial to construct, so interpreted logically, as propositions, they are completely uninteresting.

The same goes for BoolAndNat, ThreeBools, and TwoNats. They are all trivially "true" propositions.

Pair, on the other hand, contains values of generic type parameters, and therefore cannot always be trivially constructed:

-}

-- absurd-pair : {A B : Set} → Pair A B
-- absurd-pair = {!   !}

{-

The above function is once again an example of an absurdity, like "{A : Set} → A", a concept which we briefly covered in Ch0.

It is commented out to indicate that you aren't expected to fill it in, but try commenting it back in to understand why it is impossible.

In fact, even if you have one of the two elements of the pair, you still can't construct a full pair:

-}

-- absurd-pair-2 : {A B : Set} → A → Pair A B
-- absurd-pair-2 x = {!   !}

-- absurd-pair-3 : {A B : Set} → B → Pair A B
-- absurd-pair-3 x = {!   !}

{-

There is exactly one situation in which it is possible to construct a pair, for any two type parameters.

Specifically, the situation in which you have an instance of each of its type parameters:

-}

construct-pair : {A B : Set} → A → B → Pair A B
construct-pair x y = makePair x y

{-

So, "Pair A B" can be logically interpreted as the proposition "A and B".

Naturally, "A and B" implies A, and also implies B:

-}

prove-left : {A B : Set} → Pair A B → A
prove-left x = fst x

prove-right : {A B : Set} → Pair A B → B
prove-right x = snd x

{-

And we can actually prove more or less any property that you would expect to be true of "and":

-}

-- A implies A and A
boring-Pair : {A : Set} → A → Pair A A
boring-Pair x = makePair x x

-- If A implies C, then A and B implies C and B
map-left : {A B C : Set} → (A → C) → Pair A B → Pair C B
map-left f (makePair x y) = makePair (f x) y

-- If B implies C, then A and B implies A and C
map-right : {A B C : Set} → (B → C) → Pair A B → Pair A C
map-right f (makePair x y) = makePair x (f y)

{-

And we can also finally show that curried functions are the same as the typical tupled functions which most languages use:

-}

curry : {A B C : Set} → (Pair A B → C) → A → B → C
curry f x y = f (makePair x y)

uncurry : {A B C : Set} → (A → B → C) → Pair A B → C
uncurry f (makePair x y) = f x y

{-

Simultaneously, this serves as a proof that saying "(A and B) implies C" is the same as saying "A implies (B implies C)".

We will prove more such theorems later in this chapter.

Until then, let's look at another interesting type that we can (sort of) define with records:

-}

record RawBijection (A : Set) (B : Set) : Set where
  constructor makeRawBijection
  field
    to : A → B
    from : B → A

open RawBijection

{-

In mathematics, a Bijection is a mapping between two sets which assigns exactly one element in one set to exactly one element in the other set.

This is also often referred to by programmers as an Isomorphism, which is actually a more general term that is used with more than just Sets/types.

Being able to construct a bijection is effectively proof that two sets have the same number of elements.
  (In fact, some will even refer to this as an equivalence between types, and that a Bijection demonstrates that two types are the same)

However, this data type only expects a "to" and "from" function, and does not enforce that they form a bijection together.

We might normally express this requirement more formally by saying something like:
  "∀ a ∈ A, a ≡ from (to a)" (every element in set A has a one-to-one mapping in set B)
  and
  "∀ b ∈ B, b ≡ to (from b)" (every element in set B has a one-to-one mapping in set A)

We haven't learned how to enforce such a requirement in the Agda type system yet, so for now we will just do what most programmers do:

Its a pretty common practice in programming to have informal preconditions, post-conditions, and invariants about variables.

For example, we might require and assume that the numerator and denominator of some particular "Fraction" data type always be in reduced form.

In the same way, we are going to require informally that any RawBijection follow the above mentioned laws.

In Agda, since we are perfectly capable of enforcing an invariant like this in the type system, we like to prefix types like this with "Raw".

This is useful for practical programming in Agda when we just want to pump some code out without having to deal with formal correctness proofs.

So, make sure you don't construct any unlawful RawBijections, and you can assume that all RawBijections you are handed are lawful as well.

For these exercises, focus on proving these laws informally but clearly in your head (or even on paper).

-}

identity-Bool-Bijection : RawBijection Bool Bool
identity-Bool-Bijection = makeRawBijection id id

inverse-Bool-Bijection : RawBijection Bool Bool
inverse-Bool-Bijection = makeRawBijection not not

FlippedPear-Bijection : {A B : Set} → RawBijection (Pair A B) (Pair B A)
FlippedPear-Bijection = makeRawBijection swap swap

TwoOf-Bijection : {A : Set} → RawBijection (Pair A A) (Bool → A)
TwoOf-Bijection {A} = makeRawBijection (λ { (makePair x y) false → x
                                          ; (makePair x y) true → y })
                                       (λ f → makePair (f false) (f true))

flip-Bijection : {A B : Set} → RawBijection A B → RawBijection B A
flip-Bijection f = makeRawBijection (from f) (to f)

compose-Bijection : {A B C : Set} → RawBijection A B → RawBijection B C → RawBijection A C
compose-Bijection {A} {B} {C} f g = makeRawBijection to' from'
  where
    to' : A → C
    to' x = to g (to f x)
    from' : C → A
    from' x = from f (from g x)

{-

Filling in a record containing functions can sometimes get a bit clunky, so it is worth defining each "to" and "from function on separate lines.

You can just write these as global functions, but you could also write them in a 'where' clause (or even with 'let' bindings).

If you use where/let and need to write a type signature, make sure you bring any used type variables into scope like:

foo : {A B : Set} → ...
foo {A} {B} ... = ... -- Here we have brought A and B into scope
  where
    helper-function : ... A ... B ... -- Without A and B in scope, you would get an error here
    helper-function ... = ...

However if you really want to write a whole definition as a single expression, Agda has a syntax for pattern matching lambdas:

If you have a lambda like "λ x → ...", wrap everything after the 'λ' in curly braces, like "λ { x → ... }".

Now you can use C-c C-c to case split on the variables of the lambda, and Agda's interactive mode will take care of all of the monkey work for you.

Don't overuse this though and remember to keep your code readable and well organized.

Also, you are generally free to write expressions on multiple lines, as long as you indent appropriately.

Now, why don't we finally answer the big question of the chapter, which may or may not have been bothering you until now:

Why the word "product"? What do "products" have to do with pairs and tuples?

"Pair A B", which we refer to as the product of "A" and "B", is the *cartesian* product of the Set of A, and the Set of B.

Said more simply, if A has n distinct elements, and B has m distinct elements, then Pair A B has n × m distinct elements.

This is because, to enumerate the elements of Pair A B, we have to enumerate the elements of B exactly once for each element of A (or vice versa).

So, if we enumerate the n elements of A, and then for each such element we enumerate the m elements of B, then we have enumerated n × m elements.

To better convince ourselves of this, lets define some simple data types corresponding to a few small numbers:

-}

data One : Set where
  1/One : One

data Two : Set where
  1/Two 2/Two : Two

data Three : Set where
  1/Three 2/Three 3/Three : Three

data Four : Set where
  1/Four 2/Four 3/Four 4/Four : Four

data Five : Set where
  1/Five 2/Five 3/Five 4/Five 5/Five : Five

data Six : Set where
  1/Six 2/Six 3/Six 4/Six 5/Six 6/Six : Six

-- Unlike record fields, datatype constructors automatically go into the global namespace, hence the naming scheme.

{-

These data types are similar to Bool, but they each have a number of distinct elements exactly equal to their name.

We use some new syntactic sugar here which lets us write constructors with the same type on the same line.

So "Three" for instance could have been written without syntactic sugar like:

  data Three : Set where
    1/Three : Three
    2/Three : Three
    3/Three : Three

Other than this syntactic sugar, in fact, our newly defined "Two" is exactly the same as the Booleans.

First lets define a few operations (with pattern matching of course) on these types to get used to them:

-}

Six-to-Nat : Six → ℕ
Six-to-Nat 1/Six = 1
Six-to-Nat 2/Six = 2
Six-to-Nat 3/Six = 3
Six-to-Nat 4/Six = 4
Six-to-Nat 5/Six = 5
Six-to-Nat 6/Six = 6

2/Three-and-4/Five : Pair Three Five
2/Three-and-4/Five = makePair 2/Three 4/Five

next-element-of-Four : Four → Four
next-element-of-Four 1/Four = 2/Four
next-element-of-Four 2/Four = 3/Four
next-element-of-Four 3/Four = 4/Four
next-element-of-Four 4/Four = 1/Four

One-to-Two : One → Two
One-to-Two 1/One = 1/Two

Nat-to-One : ℕ → One
Nat-to-One n = 1/One

Bool-is-Two : RawBijection Bool Two
Bool-is-Two = makeRawBijection (λ { false → 1/Two
                                  ; true → 2/Two })
                                λ { 1/Two → false
                                  ; 2/Two → true }

{-

Notice how One in particular behaves quite strangely, as its elements contain absolutely no information, and are always equal to 1/One.

One is somewhat like a degenerate case of product types, as it is the "empty" product, i.e. the product of zero things.

Let's look now, at what happens when we create a Pair of two of these types.

We will use Bijections to count how many elements are in a type:

-}

-- Fill in the hole in this type signature with one of the types we just defined
two-times-two-is-? : RawBijection (Pair Two Two) Four
-- And now demonstrate this to be true in the definition
two-times-two-is-? = makeRawBijection (λ { (makePair 1/Two 1/Two) → 1/Four
                                         ; (makePair 1/Two 2/Two) → 2/Four
                                         ; (makePair 2/Two 1/Two) → 3/Four
                                         ; (makePair 2/Two 2/Two) → 4/Four })
                                       λ { 1/Four → makePair 1/Two 1/Two
                                         ; 2/Four → makePair 1/Two 2/Two
                                         ; 3/Four → makePair 2/Two 1/Two
                                         ; 4/Four → makePair 2/Two 2/Two }
-- Also, don't forget to prove to yourself that this is indeed a lawful bijection

three-times-?-is-six : RawBijection (Pair Three Two) Six
three-times-?-is-six = makeRawBijection (λ { (makePair 1/Three 1/Two) → 1/Six
                                           ; (makePair 1/Three 2/Two) → 2/Six
                                           ; (makePair 2/Three 1/Two) → 3/Six
                                           ; (makePair 2/Three 2/Two) → 4/Six
                                           ; (makePair 3/Three 1/Two) → 5/Six
                                           ; (makePair 3/Three 2/Two) → 6/Six })
                                         λ { 1/Six → makePair 1/Three 1/Two
                                           ; 2/Six → makePair 1/Three 2/Two
                                           ; 3/Six → makePair 2/Three 1/Two
                                           ; 4/Six → makePair 2/Three 2/Two
                                           ; 5/Six → makePair 3/Three 1/Two
                                           ; 6/Six → makePair 3/Three 2/Two }

-- There are two valid (though more or less equivalent) solutions here
?-times-?-is-five : RawBijection (Pair One Five) Five
?-times-?-is-five = makeRawBijection (λ x → snd x)
                                      λ x → makePair 1/One x

-- We can even show some laws of multiplication
multiplicative-identity : {A : Set} → RawBijection (Pair A One) A
multiplicative-identity = makeRawBijection (λ x → fst x) 
                                            λ x → makePair x 1/One

{-

This concept is pretty nice to understand, as products are used incredibly often in programming.

It also justifies our logical interpretation of Pair A B, as the cartesian product of A and B is nonempty if and only if both A *and* B are nonempty.

-}


-- practice exercises:


BoolAndNat-Bijection : RawBijection BoolAndNat (Pair Bool ℕ)
BoolAndNat-Bijection = makeRawBijection (λ x → makePair (getBool x) (getNat x))
                                        (λ x → makeBoolAndNat (fst x) (snd x))

big-Bijection : RawBijection BoolAndNat ℕ
big-Bijection = makeRawBijection to' from'
  where
    to' : BoolAndNat → ℕ
    to' (makeBoolAndNat false n) = 2 * n
    to' (makeBoolAndNat true n) = 2 * n + 1
    open import Solutions.Ch2 using (isOdd ; halve)
    from' : ℕ → BoolAndNat
    from' n = makeBoolAndNat (isOdd n) (halve n)

TwoBijection-Bijection : RawBijection Two (RawBijection Two Two)
TwoBijection-Bijection = makeRawBijection to' from'
  where
    invertTwo : Two → Two
    invertTwo 1/Two = 2/Two
    invertTwo 2/Two = 1/Two

    to' : Two → RawBijection Two Two
    to' 1/Two = makeRawBijection id id
    to' 2/Two = makeRawBijection invertTwo invertTwo
    
    from' : RawBijection Two Two → Two
    from' f = to f 1/Two
    

Bijection-Bijection : {A B : Set} → RawBijection (RawBijection A B) (RawBijection B A)
Bijection-Bijection = makeRawBijection flip-Bijection flip-Bijection


-- As "Pair A B" is analogous to multiplication (i.e. A × B), "A → B" is analogous to Exponentiation (i.e. Bᴬ)

first-power-Bijection : {A : Set} → RawBijection A (One → A)
first-power-Bijection {A} = makeRawBijection to' from'
  where
    to' : A → One → A
    to' x 1/One = x
    from' : (One → A) → A
    from' f = f 1/One

second-power-Bijection : RawBijection TwoNats (Two → ℕ)
second-power-Bijection = makeRawBijection to' from'
  where
    to' : TwoNats → Two → ℕ
    to' x 1/Two = TwoNats.fst x
    to' x 2/Two = TwoNats.snd x
    from' : (Two → ℕ) → TwoNats
    from' f = makeTwoNats (f 1/Two) (f 2/Two)

third-power-Bijection : RawBijection ThreeBools (Three → Bool)
third-power-Bijection = makeRawBijection to' from'
  where
    to' : ThreeBools → Three → Bool
    to' x 1/Three = ThreeBools.fst x
    to' x 2/Three = ThreeBools.snd x
    to' x 3/Three = ThreeBools.thd x
    from' : (Three → Bool) → ThreeBools
    from' f = record { fst = f 1/Three ; snd = f 2/Three ; thd = f 3/Three }

fourth-power-Bijection : {A : Set} → RawBijection (Pair A (Pair A (Pair A A))) (Four → A)
fourth-power-Bijection {A} = makeRawBijection to' from'
  where
    to' : Pair A (Pair A (Pair A A)) → Four → A
    to' x 1/Four = fst x
    to' x 2/Four = fst (snd x)
    to' x 3/Four = fst (snd (snd x))
    to' x 4/Four = snd (snd (snd x))
    from' : (Four → A) → Pair A (Pair A (Pair A A))
    from' f = makePair (f 1/Four) (makePair (f 2/Four) (makePair (f 3/Four) (f 4/Four)))

TupleOf : ℕ → Set → Set
TupleOf zero A = One
TupleOf (suc n) A = Pair A (TupleOf n A)

sixth-power-Bijection : {A : Set} → RawBijection (TupleOf 6 A) (Six → A)
sixth-power-Bijection {A} = makeRawBijection to' from'
  where
    to' : TupleOf 6 A → Six → A
    to' (makePair a (makePair b (makePair c (makePair d (makePair e (makePair f g)))))) 1/Six = a
    to' (makePair a (makePair b (makePair c (makePair d (makePair e (makePair f g)))))) 2/Six = b
    to' (makePair a (makePair b (makePair c (makePair d (makePair e (makePair f g)))))) 3/Six = c
    to' (makePair a (makePair b (makePair c (makePair d (makePair e (makePair f g)))))) 4/Six = d
    to' (makePair a (makePair b (makePair c (makePair d (makePair e (makePair f g)))))) 5/Six = e
    to' (makePair a (makePair b (makePair c (makePair d (makePair e (makePair f g)))))) 6/Six = f
    from' : (Six → A) → TupleOf 6 A
    from' f = makePair (f 1/Six) (makePair (f 2/Six) (makePair (f 3/Six) (makePair (f 4/Six) (makePair (f 5/Six) (makePair (f 6/Six) 1/One)))))


-- We can also church encode product types

church-encode-BoolAndNat : BoolAndNat → {R : Set} → (Bool → ℕ → R) → R
church-encode-BoolAndNat (makeBoolAndNat b n) = λ f → f b n

church-decode-BoolAndNat : ({R : Set} → (Bool → ℕ → R) → R) → BoolAndNat
church-decode-BoolAndNat x = makeBoolAndNat (x (λ b n → b)) (x (λ b n → n))

church-encode-Pair : {A B : Set} → Pair A B → {R : Set} → (A → B → R) → R
church-encode-Pair (makePair x y) = λ f → f x y

church-decode-Pair : {A B : Set} → ({R : Set} → (A → B → R) → R) → Pair A B
church-decode-Pair x = makePair (x (λ a b → a)) (x (λ a b → b))

church-fst : {A B : Set} → ({R : Set} → (A → B → R) → R) → A
church-fst x = x (λ a _ → a)

church-snd : {A B : Set} → ({R : Set} → (A → B → R) → R) → B
church-snd x = x (λ _ b → b)

church-swap : {A B : Set} → ({R : Set} → (A → B → R) → R) → {R : Set} → (B → A → R) → R
church-swap x = λ f → f (x (λ a b → b)) (x (λ a b → a))


-- Bi-implicational, aka "if and only if", written with "\<=>" or "\iff"
-- This definition is effectively the same as a RawBijection, but without the informally written expectation that it is a lawful Bijection.
-- Bi-implication merely says that two sets "imply" each other when they are interpreted as propositions.
_⇔_ : Set → Set → Set
A ⇔ B = Pair (A → B) (B → A)

_iff_ = _⇔_


-- We can turn a Bijection into a bi-implication, but not the other way around.
-- RawBijection makes an informal promise about the functions it contains, whereas _⇔_ does not.
Bijection-to-iff : {A B : Set} → RawBijection A B → A ⇔ B
Bijection-to-iff f = makePair (to f) (from f)

-- For instance, the none of the following could be a valid Bijection:

One⇔Two : One ⇔ Two
One⇔Two = makePair (λ _ → 1/Two) (λ _ → 1/One)

idempotent-Pair : {A : Set} → A ⇔ Pair A A
idempotent-Pair = makePair (λ x → makePair x x) (λ x → fst x)

Pair-to-iff : {A B : Set} → Pair A B → (A ⇔ B)
Pair-to-iff x = makePair (λ _ → snd x) (λ _ → fst x)

[A∧[A→B]]⇔[B∧[B→A]] : {A B : Set} → Pair A (A → B) ⇔ Pair B (B → A)
[A∧[A→B]]⇔[B∧[B→A]] = makePair (λ { (makePair a a-to-b) → makePair (a-to-b a) (λ _ → a)})
                              λ { (makePair b b-to-a) → makePair (b-to-a b) (λ _ → b)}


-- Given an impossible Bijection, construct a Bool which could neither be true nor false according to the laws of Bijections
unlawful-Bijection-to-unlawful-Bool : RawBijection ℕ Bool → Bool
unlawful-Bijection-to-unlawful-Bool (makeRawBijection to from) = to (suc (from false + from true))


{-

open-ended exercises:

Define RawInjection and RawSurjection:
  Write their laws in a comment
  Define some example Injections and Surjections
  Can they be composed?
  Can they be flipped?
  What properties do they have when one of their type arguments is One or Two?
  What law-respecting operations can be done with them?
  How many of them are there when applied to specific types (i.e. One, Two, ℕ) as well as generic type parameters?

-}


-- challenge exercises:

-- if k ≠ 0 implies x = y, then k × x = k × y
lemma₁ : {X Y K : Set} → (K → RawBijection X Y) → RawBijection (Pair K X) (Pair K Y)
lemma₁ k→[x↔y] = makeRawBijection (λ { (makePair k x) → makePair k (to   (k→[x↔y] k) x) })
                                  (λ { (makePair k y) → makePair k (from (k→[x↔y] k) y) })

lemma₂ : ∀ {A B C : Set} → RawBijection A B → RawBijection (RawBijection B C) (RawBijection A C)
lemma₂ a↔b = makeRawBijection (λ b↔c → compose-Bijection a↔b b↔c)
                              (λ a↔c → compose-Bijection (flip-Bijection a↔b) a↔c)

-- Try to make it as clear as possible that your definition respects the laws of Bijections
-- One way to do this is to define smaller helper functions which can be reasoned about more clearly
Bijection-transitivity-Bijection : {A B C : Set} → RawBijection (Pair (RawBijection A B) (RawBijection B C))
                                                                (Pair (RawBijection A B) (RawBijection A C))
Bijection-transitivity-Bijection = lemma₁ lemma₂


ThreeBijection-Bijection : RawBijection Six (RawBijection Three Three)
ThreeBijection-Bijection = makeRawBijection to' from'
  where

    next prev swap-2-3 swap-1-2 swap-1-3 : Three → Three

    next 1/Three = 2/Three
    next 2/Three = 3/Three
    next 3/Three = 1/Three

    prev x = next (next x)

    swap-2-3 1/Three = 1/Three
    swap-2-3 2/Three = 3/Three
    swap-2-3 3/Three = 2/Three

    swap-1-2 x = next (swap-2-3 x)

    swap-1-3 x = next (swap-1-2 x)

    to' : Six → RawBijection Three Three
    to' 1/Six = makeRawBijection id id
    to' 2/Six = makeRawBijection next prev
    to' 3/Six = makeRawBijection prev next
    to' 4/Six = makeRawBijection swap-2-3 swap-2-3
    to' 5/Six = makeRawBijection swap-1-3 swap-1-3
    to' 6/Six = makeRawBijection swap-1-2 swap-1-2

    from'-helper : Three → Three → Six
    from'-helper 1/Three 2/Three = 1/Six
    from'-helper 2/Three 3/Three = 2/Six
    from'-helper 3/Three 1/Three = 3/Six
    from'-helper 1/Three _       = 4/Six
    from'-helper 2/Three _       = 5/Six
    from'-helper 3/Three _       = 6/Six 
     
    from' : RawBijection Three Three → Six  
    from' f = from'-helper (to f 1/Three) (to f 2/Three)  