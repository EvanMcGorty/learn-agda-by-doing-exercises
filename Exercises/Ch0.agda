{-

Welcome. This is an Agda multi-line comment.

Make sure your setup can display 150 character lines in a way that is comfortable for you to read (e.g. word-wrap).

Each chapter of this tutorial starts by introducing some concepts or language features, along with simple definitions for the reader to fill in.

After this, there is a large set of exercises in the form of incomplete definitions, a few open-ended exercises, and then some extra-hard challenges.

This chapter is just going to zoom through some of the formalities and basics in Agda.

It is not at all necessary to fully internalize everything in this chapter before moving on, but just be prepared to reference it as necessary.

You will also re-practice everything here throughout the next chapters, with more concrete and motivating use-cases.

Now, lets get started:

Press "C-c C-l". This is Agda speak for "Press CTRL c followed by CTRL l". Agda commands usually start with CTRL c.

If you successfully set up Agda with your editor of choice, then this command has loaded this Agda file and your text now has highligting.

You should also see a window pop up next to the editor. This window interactively provides information about types in your Agda environment.

In Agda, we load files manually, as typechecking (upon which code highlighting can depend) can involve executing expensive computations.

We will start out with some options that we will be using throughout this tutorial.

-}

{-# OPTIONS --safe --cubical-compatible #-}

{-

--safe means that all code must terminate and requires you to only use a safe subset of Agda which is logically sound.

--cubical-compatible stops the typechecker from using certain axioms that are logically inconsistent with other variants of Agda.

These options more or less maximize compatibility with files using other extensions of Agda.

Now we will declare a module for this file, allowing its contents to be accessed from other files.

-}

module Exercises.Ch0 where

{-

If you want to declare a module, it has to match the file name, and, in a library (like this), it needs to contain the file's exact path.

Agda has support for unicode, which will be used in this tutorial but will always be optional for the reader to type.

To type something in unicode, you press "\", and then type in the name corresponding to the desired symbol.

For example, "→", the function arrow, is typed as "\to" or "\->", and has an ascii synonym "->"

If you wish to know how to type something in unicode, highlight it and type C-x C-= (CTRL x and then CTRL =)

Now, get ready for a type!

-}

-- can be read as "the type of id is: forall A (in Set), A to A"
id : {A : Set} → A → A

{-

This is the first part of the definition of "id", sometimes referred to as a "type signature", and it says the following:

"id" takes an implicit/inferred argument which is a Set, aka a Type, and in this context we give it the name "A"

This produces a function of type "A → A", i.e. a function which takes an A and returns an A.

Computationally, this is the type of the polymorphic identity function, polymorphic meaning that it can be used on a value of any type

This means that if we had a typical "Int" type, then we could use id with it like "id 7", which would presumably result in "7".

Agda doesn't come with built in data types, so you can either use a library, or define them yourself, which we will do in the coming chapters.

So, this chapter will focus on polymorphic functions, which you will be able to use on the data types defined in future chapters.

Thinking of typical data types and functions as "Sets" (a la Set Theory) can be useful for formally reasoning about programs.

However, the fact that we denote types with the identifier "Set" is not particularly significant (and it can even be renamed if necessary).

Types in programming have a lot in common with Sets from Set Theory, but "Set"s in Agda are not exactly the same as "Sets" in Set Theory.

Now, lets look at the definition of id:

-}

-- can be read as "id of x equals ..."
id x = {!   !}
-- notice how we write "id x" rather than the more typical syntax "id(x)".

{-

What's that weird curly-brace thing? That's a hole. It means you haven't defined the function yet, but still wanted to load the file.

Being a functional programming language, Agda doesn't really have return statements or other imperative statements.

Instead, we just define functions to be equal to expressions which use the function's arguments.

This might seem strange or restrictive at first, but ends up being very pleasant when you get used to it, and has a number of advantages.

You can fill in the hole by typing something into it, and pressing C-c C-space (as long as your cursor is still inside of it).

If you typed something invalid into the hole (i.e. it doesn't typecheck), you will get an error message explaining why.

If the hole is empty, you can also press C-c C-a (again with your cursor in the hole) to ask Agda to try to fill it in with a solution.

For the sake of learning, C-c C-a should be avoided. It is best used as a tool to help you minimize the typing-in of obvious solutions.

To place a hole, you dont have to actually type out "{!!}", you can just type "?" and reload the file.

Holes can be placed anywhere in an expression or type, even multiple times, and are a way of saying "so far, this expression typechecks".

You can also inspect the context in the hole with C-c C-, to see what is available to be used in it.

Finally, C-c C-. lets you inspect the type of the expression currently typed into the hole, in addition to the context.

You can jump into the next or previous hole in a file with C-c C-f and C-c C-b respectively (which is espcially nice when using vim or emacs).

Take a second to try all of this out. Holes, and interactive commands are really nice to work with once you get used to them.

Note:
  Remember to reload the file whenever you make a change by hand.
  Agda's interactive commands are not aware of any changes you made since the last reload.
  This can lead to confusing issues if you forget to reload before running a command.
  Also, don't forget that your cursor has to be in a hole for most commands.

Lets look at another way to define id:

-}

-- Here we are putting the type signature and the definition of id2 right next to each other
-- This is not required but is more typical.
id2 : {A : Set} → A → A
id2 {A} x = {!   !}

{-

This time, we have introduced the generic type argument in the definition of id2, bringing it into scope.

In Agda, functions can be applied to implicit arguments with curly braces, i.e. "myPolymorphicFunction {myTypeParameter} ...".

Here, you could define id2 in terms of id, by simply filling the hole in with "id x", and the type parameter would be inferred automatically.

However, now that "A" is in scope at the definition, you could also explicitly apply the implicit parameter, like "id {A} x".

You can also place a hole in the type parameter like "id {?} x", and use C-c C-s to "solve" this type constraint, if it can be inferred.

It's critical though, that we introduce the implicit parameter like we have here, as the definition is completely independent from the type signature.

Identifiers in scope in the type signature have no relation to those in scope at the definition.

In fact, we don't even have to give it the same name in the definition as we did in the type signature:

-}

id3 : {A : Set} → A → A
id3 {SomeReallyCoolType} x = {!   !}

{-

If you inspect the context in the hole, you will see that Agda now plugs the introduced name into the polymorphic type from the type signature.

In most more typical programming languages, functions are not defined like this, and id would probably be written something like:

def id<A>(A x) -> A {
  return x
}

Here, the type of id3 and the definition of id3 are mixed together, which can make it a bit easier to read initially.

There are a number of advantages, however, to drawing a more strict distinction between types and definitions.

For instance, if we are only interested in defining one function directly in terms of another, then there is a more direct way of doing this:

-}

id4 : {A : Set} → A → A
id4 = {!   !}

{-

In Agda, as in many functional programming languages, we say that functions are "first-class citizens", meaning that they are just regular values.

This means that we can define one function to simply be equal to another function of the same exact type, without threading around parameters.

In most programming languages, a function is a special kind of declaration, but in Agda, functions are no more special than Ints, or Strings.

In fact, unlike in the vast majority of languages, even polymorphic functions are "first-class", and can be passed around like regular objects.

All of this allows us to give every identifier a clear and unambiguous type, which is completely separate from its definition.

Types are very special in Agda, as, among other things, we can often interpret them as logical propositions.

This actually can be done to at least *some* degree in the majority typed programming languages with polymorphism.

The idea behind this is to conceptually collapse Sets down so that we only care about whether they are empty or inhabited (aka nonempty).

In our case, "Sets" are our types, and a type is inhabited when we can construct a value of it, and empty when this is impossible.

If the nonemptyness of a Set is equivalent to a meaningful proposition, then constructing element of that Set is a proof of said proposition.

So when we "interpret" a Set/Type as a proposition, we simply understand it to be false if it is empty, and true if it is inhabited.

A function from A to B can then be interpreted as a proof that "the proposition corresponding to A implies the proposition corresponding to B".

We justify this by the fact that the Set of all functions from A to B is nonempty if and only if A being nonempty implies that B is nonempty.

In this case, we can interpret "id" as a proposition that forall A, A implies A.

This can be very confusing at first and is not critical to completely understand, so don't worry if you don't quite get it yet.

The idea of an empty Type, i.e. a Type which you cannot construct a value of, also may sound strange, and is foreign to most programming languages.

However we will soon see examples of "empty" types, and you will get used to all these ideas intuitively just by working through this tutorial.

Moving on, here is another definition for you to fill in:

-}

const : {A : Set} → {B : Set} → A → B → A
const x y = {!   !}

{-

This function, for any two types "A" and "B", takes an "A" which it calls "x", and a "B" which it calls "y", and then returns an "A".

At this point however, you are probably wondering why function types are written so strangely with multiple "→"s.

In a more typical language, you would expect to see something like "(A, B) → A", i.e. "A and B to A", instead of "A → B → A", i.e. "A to B to A".

However, these two forms are effectively equivalent, and using the latter in place of the former is often referred to as "currying".

Syntactically, "→" associates to the right, which is another way of saying that "A → B → A" is understood as "A → (B → A)" rather than "(A → B) → A".

So, an "A → B → A" (aka "A → (B → A)") is actually a function which takes an "A" and returns another function, which takes a "B" and returns an "A".

Effectively, you have something that you can give an "A", and then give a "B", and get back an "A".

This is in practice no different than something which you can give an "A" and a "B" to get back an "A".

This can also be understood logically as the equivalence between "A implies (B implies A)" and "(A and B) implies A".

So, in practice, "const" can logically be interpreted as a proof of either of these two equivalent propositions.

For this to work syntactically, its also important that "const x y" is understood as "(const x) y", and not "const (x y)".

In a more traditional language, this would be like writing "const(x)(y)", which is a bit ugly, and is the main reason that we don't use this syntax.

Currying is nice because it eliminates redundancy, and lets us have a single, universal function type, compatible with any number of arguments.

For example, if we have "myThing : A", and "foo : (A, B, C, D) → E", then we could write the following definition:

  bar : (B, C, D) → E
  bar (x, y, z) = foo (myThing, x, y, z)

In Agda, where we curry our functions, we would have "foo : A → B → C → D → E", and we would define bar like:

  bar : B → C → D → E
  bar x y z = foo myThing x y z

However, there is an advantage of the curried variant of bar, namely that its definition can be simplified to:

  bar = foo myThing

This can be justified by the fact that, for any two functions f and g, f = g if and only if f x = g x (this law is called "function extensionality").

So, we simply apply this rule repeatedly for each individual curried argument:

  bar x y z = foo myThing x y z
  
  bar x y = foo myThing x y
  
  bar x = foo myThing x
  
  bar = foo myThing

Which is perhaps a bit clearer when we insert parentheses to emphasize the order in which curried functions are applied to arguments:

  ((bar x) y) z = (((foo myThing) x) y) z
  
  (bar x) y = ((foo myThing) x) y
  
  bar x = (foo myThing) x
  
  bar = foo myThing
  
The equivalence of these forms is sometimes called η-equality ("eta-equality") and simplifying functions like this is called η-reduction.

Currying and η-equality can be very confusing at first, so take your time to process this if it's your first time learning about them.

Being a functional language, we use functions a *lot* in Agda, so these concepts are very handy, and are worth understanding and practicing.

Try using this to fill in the following definition:

-}

const2 : {A : Set} → {B : Set} → A → B → A
const2 x = {!   !}

{-

Here, we have only introduced one of the two necessary parameters.

Despite this, we can use our first definition of const along with η-equality to fill in the hole without introducing another parameter.

Now, lets look at another way of defining const:

-}

const3 : {A : Set} {B : Set} → A → B → A

{-

Not much has changed yet, but noteworthy is some syntactic sugar:

Instead of "{A : Set} → {B : Set} → ...", you can leave out the "→" between named parameters at the beginning of a type.

Now get ready for some new syntax:

-}

const3 x = λ y → {!   !}

{-

So, what the hell is going on here? The expression to the right of the "=" is called a lambda, which is more or less an anonymous function definition.

"λ" is the greek letter lambda, and can be typed as "\Gl" (and can be remembered as "Greek l"), or also simply as "\lambda".

Those who want to avoid unicode can simply use a backslash in place of "λ", which is typed as "\\" if unicode typing is enabled in your editor.

Also, be careful not to type "\GL" or "\Lambda", or you will end up with uppercase lambda, i.e. "Λ".

The idea behind lambdas is the following:

  If I have a function f, it is usually defined in a way that lets me see what f x is. But what is f itself?

  Well, lambda syntax proposes an answer to this question: If "f x = ...", then "f = λ x → ..."

So, thanks to currying, the above definition of const3 is completely equivalent to the definition of const.

Let's explore just one more way of defining const:

-}

const4 : {A B : Set} → A → B → A

{-

Once again, nothing too important here, but a bit more syntactic sugar which is worth remembering:

Because A and B are both "Sets", we can write them within the same parentheses/braces, though in reality they are still separate parameters.

-}

const4 = λ x → {!   !}

{-

This time, we have only introduced one of the two arguments, so the value which you need to fill in is itself a function.

Now, here we could simply modify the lambda to take two arguments, or add one of the arguments before the "=".

However, that would be too easy, so try to define the rest of the function by only filling in the hole.

Moving on, lets look at some functions that themselves take other functions as parameters (often referred to as "higher order functions").

-}

-- You can put parentheses around the "A → C" to make this type (subjectively) more readable
-- The fact that we don't need these parenthases is an interesting effect of currying.
-- This shows that returning a whole other function is no different than just introducing another parameter
-- When you curry, you are already always just returning a function anyway
compose : {A B C : Set} → (B → C) → (A → B) → A → C
compose f g = {!   !}

{-

Function composition is a pretty classic operation, and is hard to get wrong. Nonetheless I urge you to be careful with the syntax in Agda.

As "→" associates to the right to allow for curried function types, function application associates to the left to allow curried function application.

This effectively means that "foo x y z" is understood as "((foo x) y) z". This is great, but it can catch you off guard.

If you write "increment decrement x", for instance, Agda doesn't know that you meant "increment (decrement x)", and thinks that you:
  1. wanted to increment "decrement" (which makes no sense)
  and
  2. wanted to apply the result of incrementing "decrement" to x (which also makes no sense)

Consequently you will get a possibly nasty type error, instead of being informed that you need parentheses. Try this out in compose.

Another thing you can try out here is the interactive Agda command C-c C-r, called "refine", which can do a few different things.

If the type of a goal (inside of a hole) is a function type, like "A → B", pressing C-c C-r in an empty hole introduces a parameter via a lambda.

Alternatively if the type of a goal is "A", and you have a function which returns an "A", you can type that function into the hole and press C-c C-r.

This will fill the hole with the entered function, but applied to a series of new holes, one for each necessary parameter.

Finally, you can type in a valid expression which itself contains holes (which you type in as '?'s), and refine so that only those holes remain.
  (note: this can also be done with C-c C-space)

If the new expression would not type check though, you will just get an error in the interactive window.

Be sure to practice using all of these to write definitions piece by piece while taking advantage of C-c C-, and C-c C-.

Let's also note once again the logical proposition that we have proven by defining compose:
  forall A B and C, if B implies C, and A implies B, then A implies C

We will now move on to the last concept that will be introduced in this chapter:

-}

-- absurdity : {A : Set} → A
-- absurdity = ?

-- absurdity2 : {A B : Set} → A → B
-- absurdity2 = ?

{-

The above commented out definitions are absurdities. They are impossible to define and, logically interpreted, are false propositions.

They have polymorphic types which are empty Sets, and completing their definitions would amount to constructing an element of an empty Set.

This does not mean that "A" or "A → B" is an empty Set for any A or B, but rather that the Set of all such polymorphic functions is empty.

Note:
  The idea that a *polymorphic* function type could itself also be a Set in some sense may initially sound strange or even paradoxical.
  In fact, by default in Agda, polymorphic functions like "{A : Set} → A" do not have the type Set (aka "Set₀"), but rather something called "Set₁".
  This means that you cant instanciate a Set₀-polymorphic function (e.g. any definition in this exercise) with another type which mentions Set₀.
  Don't worry too much about this detail for now, as it only really exists to prevent undesirable and paradoxical behavior in Agda.
  Why this is necessary, as well as the workaround for this will be discussed in a later chapter, but for now it's good to just be aware of it.

"absurdity" claims that for any Set, there exists an element in that Set. Logically, this is a claim that every proposition is true.

"absurdity2" claims that for any two Sets, there exists a function between those Sets, or, logically, that any two propositions imply each other.

These definitions are commented out to indicate that you, the reader, are not require to fill in their definitions to complete the exercise.

Feel free however, to comment them back in and try experimenting to understand why they are undefinable/unprovable.

Given an absurdity, you can prove or construct literally anything, even another absurdity.

This is anologous to the fact that, for any Set, there is exactly one function from the empty Set to that Set.

Functions simply map each element in the input set to an element in the output set.

So, if the input Set is empty, then you don't need to pick any elements from the output set, and you get a function for free.

This also means that you can prove that one absurdity implies another:

-}

-- Notice how the type of this is literally "(type of absurdity) → (type of absurdity2)"
absurdity→absurdity2 : ({A : Set} → A) → {A B : Set} → A → B
absurdity→absurdity2 imp1 {A} {B} a = {!   !}

{-

This definition may be pretty confusing, as its first parameter is *itself* a polymorphic function, and there are even type parameters after this.

Interestingly, with the mindset of currying, returning another polymorphic function just means introducing more generic parameters.

These are an example of higher-rank polymorphism which is very rare to see in a more typical programming language.

This means that you can use this polymorphic function with whatever type, *in the definition* of absurdity→absurdity2.

Likewise, if someone were to call absurdity→absurdity2, they would *have* to supply a polymorphic function.

After this normal parameter, we then introduce two type parameters, and one of them even re-uses the name "A".

This is fine, as the first "A" goes out of scope by the time that we bring the second "A" into scope, though it's a bit confusing.

In the definition, we introduce these implicit type arguments ({A} and {B}) which brings them into scope and allows us to refer to them.

Strictly speaking we don't need them here thanks to Agdas powerful type inference, but explicitly applying type arguments can be more readable.

Finally, I want to use this example to point out that we can use any characters other than () and {} in a name, such as the "→" here.

Before we move on to some exercises, lets take a peek at the module system in Agda:

-}

module Foo where

  return-first : {A : Set} → A → A → A
  return-first x y = {!   !}

  return-second : {A : Set} → A → A → A
  return-second x y = {!   !}

{-

Here we have a module containing two polymorphic functions which presumably each return one of their two parameters.

These can be refereneced in the global namespace as "Foo.return-first" and "Foo.return-second", or brought into scope with "open Foo".

We will go further into detail on the usage of modules as necessary, but for now, theres actually a really cool feature we are interested in:

-}

module Bar {A : Set} where

  return-first : A → A → A
  return-first x y = {!   !}

  return-second : A → A → A
  return-second x y = {!   !}

{-

Specifically, modules allow us to factor out parameters which are shared by a collection of definitions.

Now, if we "open Bar" or use Bar.whatever to access something from Bar, this parameter will simply be propagated to the accessed definition.

If we want to use these functions with a particular type, say "Int", we can even "open Bar {Int}" or define a new "module BarInt = Bar {Int}".

But not only can we factor out implicit type parameters, we can even factor out any number of regular, explicit parameters:

-}

module Baz {A : Set} (x : A) (y : A) where

  return-first : A
  return-first = {!   !}

  return-second : A
  return-second = {!   !}

{-

This differs a lot from normal parameter syntax for functions, as the variable names and types are interspersed, rather than strictly independent.

Notably, every module parameter is brought into scope throughout the entire body of the module, all at once.

Arguably, this actually has a bit more resemblance to the way that functions take parameters in most mainstream programming languages.

Of course, we can now also do "open Baz {Int} 3 7", along with plenty of cool tricks which will be discussed when we need them.

This, however, is not why we are so interested in them right now.

Rather, the ability to abstract over a set of parameters effectively lets us make assumptions, and experiment with what can be done with them.

In a tutorial like this, which tries to have fun and interesting exercises, this is useful for posing interesting challenges:

-}

module LogicPuzzles {A B C D : Set} (f : A → D → B) (g : C → B → A) (h : A → C → D) where

  puzzle1 : A → C → B
  puzzle1 a c = {!   !}

  puzzle2 : (B → C) → B → A
  puzzle2 b-to-c b = {!   !}

  puzzle3 : (A → B → C) → A → D → C
  puzzle3 a-to-b-to-c a d = {!   !}

{-

This is significant, since it also lets us postulate that some things exist, even when we may not necessarily know how to define them ourselves.

For example, we can assume that some numerical type like "Int" or "Float" exists:

-}

-- In Agda, proper indentation allows us to write nearly anything over any number lines.
-- So, we can write parameters over multiple lines to maximize readability
module Numerics {Number : Set} 
                (minus : Number → Number → Number)
                (zero : Number)
                where

  negate : Number → Number
  negate x = {!   !}

  plus : Number → Number → Number
  plus x y = {!   !}

  double : Number → Number
  double x = {!   !}

{-

Additionally, we can write _ as a module name to just abuse this parameter feature without wrapping our definitions up in a module.

This is nice when we are just trying to cut out some duplication or experimenting and don't care to think up a module name.

And finally, one nice example of something we can assume is the existence of an empty Set:

-}

module _
  {EmptySet : Set}
  -- We say that something is false or absurd if assuming it lets us prove/construct anything.
  -- Implication is merely a requirement that *if* one thing is true, *then* another thing is true.
  -- If the first thing is not true, then the second thing is irrelevant, and the implication holds trivially.
  (EmptySet-is-absurd : {A : Set} → EmptySet → A)
  -- The "A" in this polymorphic function does not stay in scope after this.
  where

  absurdity-to-EmptySet : ({A : Set} → A) → EmptySet
  absurdity-to-EmptySet absurd = {!   !}
  
  EmptySet-to-absurdity : EmptySet → {A : Set} → A
  EmptySet-to-absurdity emptySet {A} = {!   !}

  -- If A implies false, then A implies anything
  explosion : {A B : Set} → (A → EmptySet) → A → B
  explosion {A} {B} f x = {!   !}


{-

If you have made it this far, Congratulations! You have finished the tutorial section of this chapter.

The rest of this file consists mostly of explanation-free exercises to work through, practicing and building off of what was taught above.

The content in this section is somewhat optional, so you can skip exercises if you are confident in your ability to complete them.

Nonetheless, these exercises are very valuable practice, and skipping too many of them may cause you to have difficulty with later chapters.

If you manage to complete all of them, which I recommend trying to do, then you have officially completed the chapter.

Make sure to practice all of the interactive commands discussed so far:

C-c C-l     Load File
C-c C-space Fill in hole (or refine to new, more deeply nested holes written inside as '?'s)
C-c C-a     Auto Complete
C-c C-,     Show context of current hole (in-scope variables)
C-c C-.     Show context and Goal type of current hole
C-c C-f     Move to next hole after cursor
C-c C-b     Move to previous hole from the cursor
C-c C-s     Infer type (or more generally, "solve constraint")
C-c C-r     Refine (automatically itroduce lambda or automatically apply given function to new holes)

-}


apply : {A B : Set} → (A → B) → A → B
apply f x = {!   !}

s-combinator : {A B C : Set} → (A → B → C) → (A → B) → A → C
s-combinator f g x = {!   !}

flip : {A B C : Set} → (A → {!   !} → C) → B → {!   !} → C
flip f x y = f y x

flip' : {A B C D : Set} → (A → B → C → D) → A → C → B → D
flip' f x y z = {!   !}

lift : {A B C D : Set} → (B → C → D) → (A → B) → (A → C) → A → D
lift op f g = {!   !}

on : {A B C : Set} → ({!   !} → {!   !} → {!   !}) → (A → B) → A → A → C
on op f x y = op (f x) (f y)

compose' : {A B C D : Set} → (C → D) → (A → B → C) → A → B → D
compose' f g = {!   !}

owl : {A B : Set} → ((A → B) → A) → (A → B) → B
owl f g = {!   !}

map-cont : {A B C : Set} → ({!   !} → {!   !}) → ((A → C) → C) → (B → {!   !}) → {!   !}
map-cont f g = λ h → g (λ x → h (f x))

warbler-cont : {A B C : Set} → (((A → A → B) → A → B) → C) → C
warbler-cont f = {!   !}

id-id : {A : Set} → {!   !} → {!   !}
id-id {A} = id {{!   !}} (id {{!   !}})

compose-id-id :  {A : Set} → {!   !} → {!   !}
compose-id-id {A} = compose {{!   !}} {{!   !}} {{!   !}} (id {{!   !}}) (id {{!   !}})

const-id : {A B : Set} → B → A → A
const-id {A} {B} = const {{!   !}} {{!   !}} (id {{!   !}})


{-

Now please feel free to mess around and experiment.

If you need inspiration, every chapter will give a few open-ended exercises to get you started.

open-ended exercises:

Enter in an incorrect solution, e.g. "const x y = y" and look at what kinds of errors you get.

Move an implicit parameter (like {B : Set}) to different positions within a function type. When does this work? When does this not work?

Introduce too many variables in a lambda or function definition. Wrongly introduce an implicit variable. What happens? Why?

Define a function in terms of itself. What happens? Why might this be?

Make up some logical propositions and see if you can or can't prove them. If you can't prove them, can you show that they imply an absurdity?

Try to use η-reduction on a few more complicated definitions, and use functions like "id", "const", "compose", "flip", and "lift" to help.

Can you completely η-reduce all the arguments away using only other higher-order functions? This is called "point-free" style.

-}



{-

At the end of every file, you will find challenge exercises. These can be very difficult, and can even be challenging for seasoned Agda users.

However, they should technically be possible to complete with only the knowledge taught up to that point, so don't hesitate to try them out.

Having a significant amount of experience with advanced functional programming or formal mathematics may also be very helpful.

If an exercise is too hard, It may be worth coming back to them after completing later chapters and gaining more intuition and experience with Agda.

Once again, these exercises can be *hard*.

Have fun!

-}

-- challenges:


-- Don't worry if Agda is highlighting parts of the definition here
-- It simply means that Agda hasn't been able to infer the implicit parameters given the (empty) type signature
-- The solution here should use all 5 type variables
compose'-[const-apply]-flip : {A B C D E : Set} → {!   !}
compose'-[const-apply]-flip = compose' (const apply) flip


-- "((P → Q) → P) → P" is true. Strangely we can't prove it in Agda, but we *can* show that it implying an absurdity implies an absurdity
-- We will explore this phenomenon more in later chapters
[[[[p→q]→p]→p]→absurd]→absurd : {P Q : Set} → ((((P → Q) → P) → P) → {A : Set} → A) → {A : Set} → A
[[[[p→q]→p]→p]→absurd]→absurd = {!   !}


-- Define a function which applies another function to itself
-- (the definition is in a hole to prevent an ugly error message caused by the incomplete type signature)
apply-to-self : {!   !}
apply-to-self = {! λ f → f f !}


-- For simple polymorphic functions, like most of what we have defined in this exercise, lambdas (i.e. introducing arguments) are actually overkill
-- All that you really need to create any lambda term are the following two functions, called the s and k combinators:

s : {A B C : Set} → (A → B → C) → (A → B) → A → C
s = λ x y z → x z (y z)

k : {A B : Set} → A → B → A
k = λ x y → x

-- Additionally, it is very convenient to use a third combinator, i, which is actually just equivalent to s k k

i : {A : Set} → A → A
i = λ x → x

-- Use only s and k to define the following functions which are often called combinators. Do not introduce any arguments.
-- You can use other functions you define here as well as write helper functions as long as their definitions also don't introduce arguments
-- (i.e. ultimately everything is defined in terms of s and k)
-- It may help to initially use lambdas, and then transform the expression to a point where you can substitute in a combinator
-- In addition to this, take advantage of η-equality, i.e. "λ {...} x → f x" can be simplified to λ {...} → f
-- Feel free to introduce and use *type* arguments if it helps, as they can effectively serve as annotations

k' : {A B : Set} → B → A → A
k' = {!   !}

o : {A B : Set} → ((A → B) → A) → (A → B) → B
o = {!   !}

w : {A B : Set} → (A → A → B) → A → B
w = {!   !}

t : {A B : Set} → A → (A → B) → B
t = {!   !}

b : {A B C : Set} → (B → C) → (A → B) → A → C
b = {!   !}

c : {A B C : Set} → (A → B → C) → B → A → C
c = {!   !}

Φ : {A B C D : Set} → (B → C → D) → (A → B) → (A → C) → A → D
Φ = {!   !}


-- Some caveats about types and inference:

-- If you have filled in these definitions, you may notice some yellow highlighting and mysterious goals in the Agda interactive window.
-- Certain combinators, such as k, discard arguments entirely, and can end up discarding a function which takes a generic argument.
-- This is fine, but it leaves Agda wondering what that generic parameter should be instanciated with.
-- If this yellow highlighting is disturbing to your satisfaction of completing this file, you can luckily fix it.
-- Because this function is being discarded, we can instanciate this type argument with literally any type we want.
-- So, introduce any type argument to the definition, and then apply the problematic function to this type argument with curly braces.
-- for example, the i combinator can be defined as
-- i : {A : Set} → A → A
-- i = s k k
-- but the second k will end up unused and Agda's inference engine doesn't know what type to instanciate its second type argument with. So:
-- i {A} = s k (k {_} {A})
-- This fixes the issue by filling in this second type parameter with some arbitrary, irrelevant type. We only used "A" out of convenience
-- If you squint at the type signatures for s and k long enough, you can see how the first argument can be inferred, as it *must* be "A".

-- Another thing to be weary of is the fact that an expression reducing to something that is well typed doesn't mean that it itself is well typed.
-- For example the following is also a valid definition of the i combinator
-- i = s k s
-- But without using more powerful types, this is actually invalid. We can see why as soon as we start substituting the definitions:
-- i x == s k s x = k x (s x) = x
-- This reduces correctly, but in an intermediate step, there is a term "s x" that ends up getting discarded.
-- Here, s expects a function like (A → B → C), but, according to the type signature of i, x must be able to be any type.
-- This type mismatch is, naturally, already reflected in the type of the expression "s k s", which expects a function as its argument.
-- So, the following *does* typecheck:
-- i' : {A B C : Set} → (A → B → C) → (A → B → C)
-- i' = s k s

-- More generally, its worth understanding that types, such as the type of s, can sometimes simply be less expressive than their definition.
-- For example, the expression "s f g {...}" is equal to to "f {...} (g {...})".
-- However, the type signature of s is to weak to infer a different type for {...} when f is applied to it than when g is applied to it.
-- It requires that {...} can be instanciated as a *single* type, which both f and g can accept an argument of.
-- So, it is absolutely possible for "f {...} (g {...})" to typecheck, but for "s f g {...}" not to typecheck
-- Later, you will learn how to give functions more expressive types that are less prone things like this.