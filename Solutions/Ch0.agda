{-

Welcome. This is an Agda multi-line comment.

This file is just going to zoom through some of the formalities and basics in Agda.

It is not at all necessary to fully internalize all the information in this file before moving on, but be prepared to reference it as necessary.

Press "C-c C-l". This is Agda speak for "Press CTRL c, and then press CTRL l". Agda commands usually start with CTRL c.

If you successfully set up Agda with your editor of choice, then this command has loaded this Agda file and your text now has highligting.

You should also see a window pop up next to the editor. This window interactively provides information about types in your Agda environment.

In Agda, we load the file manually, as typechecking (upon which code highlighting can depend) can involve executing expensive computations.

We will start out with some options that we will be using throughout these exercises.

-}

{-# OPTIONS --safe --cubical-compatible #-}

{-

--safe means that all code must terminate and requires you to only use a safe subset of Agda which is logically sound.

--cubical-compatible stops the typechecker from using certain axioms that are logically inconsistent with other variants of Agda.

These options more or less maximize compatibility with files using other extensions of Agda.

Normally these would have to be at the top of every file, but I set them as defaults within this library so they can be left out in later chapters.

Now we will declare a module for this file, allowing its definitions to be accessed from other files.

-}

module Solutions.Ch0 where

{-

If you want to declare a module, it has to match the file name, and, in a library (like this), it needs to contain the exact path.

Agda has support for unicode, which will be used in this tutorial but will never be required for reader to use.

To type something in unicode, you press "\", and then type in the name corresponding to the desired symbol.

For example, "→", the function arrow, is typed as "\to" or "\->", and has an ascii synonym "->"

If you wish to know how to type something in unicode, highlight it and type C-x C-= (CTRL x and then CTRL =)

Now, get ready for a type!

-}

-- can be read as "the type of id is: forall A (in Set), A to A"
id : {A : Set} → A → A

{-

This is the first part of the definition of "id".

"id" takes an implicit/inferred argument which is a Set, aka a Type, and in this context we give it the name "A"

This results in a function of type "A → A", i.e. a function which takes an A and returns an A.

Computationally, this is the type of the identity function, which can be used on a value of any type (i.e. it is polymorphic or generic).

Agda functions also can often be interpreted as proofs of logical propositions.

In this case, we can interpret id as a proposition that forall A, A implies A.

In a few chapters we will dive much deeper into the connection between programs and proofs, so this chapter will just tease the concept a bit.

Now lets look at the definition of that function, aka the proof of that proposition

-}

-- can be read as "id of x equals ..."
id x = x

{-

What's that weird curly-brace thing? That's a hole. It means you haven't defined the function yet, but still wanted to load the file.

You can fill in this hole by typing something into it, making sure your cursor is in the hole, and pressing C-c C-space.

If you typed something invalid into the hole (i.e. it doesn't typecheck), you will get an error message explaining why.

If the hole is empty, you can also press C-c C-a (again with your cursor in the hole) to ask Agda to try to fill it in with a solution.

For the sake of learning, C-c C-a should be avoided. It is best used as a tool to help you minimize the typing-in of obvious solutions.

To place a hole, you dont have to actually type out "{!!}", you can just type "?" and reload the file.

Holes can be placed anywhere in an expression or type (and even multiple times), and are a way of saying "so far, this expression typechecks".

You can also inspect the context in the hole with C-c C-, to see what is available to be used in it.

Finally, C-c C-. lets you inspect the type of the expression currently typed into the hole, in addition to the context.

You can jump into the next or previous hole in a file with C-c C-f and C-c C-b respectively.

Note:
  Remember to reload the file whenever you make a change by hand.
  Agda's interactive commands are not aware of any changes you made since the last reload.
  This can lead to confusing issues if you forget to reload before running a command.
  Also, don't forget that your cursor has to be in a hole for most commands.

Normally a type and a definition are right next to each other, like:
foo : ...
foo = ...
but we are still able to put comments in between them if we wish, as will be done frequently in these exercises.

Lets look at another way to declare id:

-}


id2 : (A : Set) → A → A
id2 A x = x

{-

This time "A" is a normal, explicit argument (written with regular parenthases instead of curly braces), and must be specified manually.

This difference is entirely syntactic and for convenience and readability. In fact, these variations can emulate each other with the following syntax:
  id {A} x
  id2 _ x

This works both at the left hand side of a definition as well as when calling the function from somewhere else.

In a definition, this effectively lets you decide whether to bring an argument into scope so that you can use it on the right hand side

At a call site (i.e. a usage of the function) this lets you decide whether the parameter should be inferred based on the types of other arguments.

In function definitions, we will almost always make types (i.e. parameters of type "Set") implicit parameters as they can usually be inferred

Note:
  We didn't *have* to give "A" the same name in the type as in the definition
  In fact, we could have very well referred to "A" as x and "x" as A in the definition, and Agda wouldn't have warned us.
  Remember that when you are looking at the context of a hole, the names introduced in the definition have priority over those in the type.

Inference, either for an implicit (curly braces) parameter, or forced by an underscore, will only result in a provably unique solution.

Similarly, you can use C-c C-s in a hole to ask Agda to actually write out the unique solution that it would infer there (similarly to C-c C-a)

When something can't be inferred, the text will be highlighted in yellow and inform you what information is missing.

Inference can even be used in a type, like:
  id : {A : _} → A → A
  id2 : (A : _) → A → A

And there is even syntactic sugar for this, namely
  id : ∀ {A} → A → A
  id2 : ∀ A → A → A

"∀" can be written by typing "\all", or, if you don't like unicode, you can just write "forall".

Moving on, here is another definition for you to fill in:

-}

const : {A : Set} → {B : Set} → A → B → A
const x y = x

{-

At this point, you may be wondering why function types are written so strangely with "→".

In a more typical language, you would expect to see something like "(A, B) → A" ("A" and "B" to "A") instead of "A → B → A" ("A" to "B" to "A").

However, these two forms are effectively equivalent, and using the latter in place of the former is often referred to as "currying".

"→" Associates to the right, which is another way of saying that "A → B → A" is understood as "A → (B → A)" rather than "(A → B) → A".

So, "A → B → A" aka "A → (B → A)" is a function which takes an "A", and returns another function which takes a "B" and returns an "A".

Effectively, you have something that you can give an "A", and then give a "B", and get back an "A".

This is no different than something which you can give an "A" and a "B" to get back an "A".

This can also be understood logically as the equivalence between "A implies (B implies A)" and "(A and B) implies A".

So, from a logical perspective "const" can practically be interpreted as a proof of either of these two equivalent propositions.

Currying is mainly done for syntactic convenience. By writing all of our functions as chains of single-argument functions, we eliminate redundancy.

If I have a "(A, B) → A" (named "f") and an "A" (named "x"), and I want a "B → A" (named "g"), I effectively need to do the following:
  g y = f (x, y)

On the other hand, if we wrote "(A, B) → A" as "A → (B → A)" from the start, all we need to do is apply f to x:
  g = f x

Then we can just use "f x", and we don't need to define a separate function (but nothing stops us from still writing "g y = f x y" if we desire).

The fact that we can write "g = f x" and "g y = f x y" inerchangeable is sometimes referred to as η-equivalence ("eta-equivalence")

If this is your first time seeing something like this, please pause and take your time to process this, as currying can be very confusing at first.

Now, lets look at another way of defining const:

-}

const2 : {A : Set} {B : Set} → A → B → A

{-

Not much has changed yet, but noteworthy is some syntactic sugar:

Instead of "{A : Set} → {B : Set} → ...", you can leave out the "→" between named parameters at the start of a function type.

Now get ready for some new syntax:

-}

const2 x = λ y → x

{-

So, what the hell is going on here? The expression to the right of the "=" is called a lambda, which is more or less an anonymous function definition.

"λ" is the greek letter lambda, and can be typed as "\Gl" (can be remembered as "Greek lowercase l"), or also simply as "\lambda".

Those who want to avoid unicode can simply use a backslash in place of "λ", which is typed as "\\" if unicode typing is enabled in your editor.

Also, be careful not to type "\GL" or "\Lambda", or you will end up with uppercase lambda, i.e. "Λ".

The idea behind lambdas is the following:

  If I have a function f, it is usually defined in a way that lets me see what f x is. But what is f itself?

  Well, lambda syntax proposes an answer to this question: If "f x = ...", then "f = λ x → ..."

So, thanks to currying, the above definition of const2 is completely equivalent to the definition of const.

Let's explore just one more way of defining const:

-}


const3 : {A B : Set} → A → B → A

{-

Once again, nothing to important here, but a bit more syntactic sugar which is worth remembering:

Because A and B are both "Sets", we can write them within the same parenthases/braces.

-}

const3 = λ x → λ y → x

{-

This time, we have only introduced one of the two arguments. The value which you need to fill in is itself a function.

Now, here we could simply modify the lambda to take two arguments, or add one of the arguments before the "=".

However, that would be too easy, so try to define the rest of the function by only filling in the hole.

Moving on, lets look at some functions that themselves take other functions as parameters (often referred to as "higher order functions").

-}

-- Note: you can put parenthases around the "A → C" to make this type (subjectively) more readable
compose : {A B C : Set} → (B → C) → (A → B) → A → C
compose f g = λ x → f (g x)

{-

Function composition is a pretty classic operation, and is hard to get wrong--nonetheless I urge you to be careful with the syntax in Agda.

As "→" associates to the right to allow for curried function types, function application associates to the left to allow curried function application.

This effectively means that "foo x y z" is understood as "((foo x) y) z". This is great, but it can catch you off guard.

If you write "increment decrement x", for instance, Agda doesn't know that you meant "increment (decrement x)", and thinks that you:
  A. wanted to increment "decrement"
  and
  B. wanted to apply the result of incrementing "decrement" to x

Consequently you will get a possibly nasty type error, instead of being informed that you need parenthases. Try this out in compose.

Another thing you can try out here is the interactive Agda command C-c C-r, called "refine". This command can do a few different things.

If the type of a goal (inside of a hole) is a function type, i.e. "A → B", pressing C-c C-r in an empty hole introduces a parameter via a lambda.

Alternatively if the type of a goal is "A", and you have a function which returns an "A", you can type that function into the hole and press C-c C-r.

This will fill the hole with the entered function, but applied to a series of new holes, one for each necessary parameter.

Finally, note once again the logical proposition that we have proven by defining compose:
  forall A B and C, if B implies C, and A implies B, then A implies C

We will now move on to the last topic of this chapter:

-}

-- impossible1 : {A : Set} → A
-- impossible1 = ?

-- impossible2 : {A B : Set} → A → B
-- impossible2 = ?

{-

The above commented out definitions are absurdities. They are impossible to define and, logically interpreted, are false propositions.

They are commented out to indicate that you, the reader, are not require to fill in their definitions to complete the exercise.

Feel free however, to comment them back in and try experimenting to understand why it is unprovable.

Given an absurdity, you can prove literally anything. This also means that you can prove that one absurdity implies another:

-}

impossible1→impossible2 : ({A : Set} → A) → {A B : Set} → A → B
impossible1→impossible2 imp1 {A} {B} a = imp1 {B}


{-

Note the introduction of implicit arguments ({A} and {B}) in this definition, which brings them into scope and allows us to refer to them.

Remember, naming variables in the type is not enough to introduce them so that you can use them in the function's definition

In this example we don't need to do this, as Agda's inference engine is entirely capable of handling a simple case like this.

However, it allows us to explicitly apply implicit arguments to make the definition more readable.

This means that we can explicitly apply imp1 to a Set like A or B by writing "imp1 {MySet}"

Additionally, on the topic of readability, feel free to replace introduced variables with an "_" if you don't use them in the definition.

And finally, I want to use this example to point out that we can use any characters other than () and {} in a name, such as the "→" here.

If you have made it this far, Congratulations! You have finished the tutorial section of this chapter.

The rest of this file consists mostly of explanation-free exercises to work through, practicing and building off of what was taught above.

If you fill in all of the holes, your Agda info window should display "*All Done*".

-}


apply : {A B : Set} → (A → B) → A → B
apply f x = f x

s-combinator : {A B C : Set} → (A → B → C) → (A → B) → A → C
s-combinator f g x = f x (g x)

flip : {A B C : Set} → (A → B → C) → B → A → C
flip f x y = f y x

flip2 : {A B C D : Set} → (A → B → C → D) → A → C → B → D
flip2 f x y z = f x z y

lift : {A B C D : Set} → (B → C → D) → (A → B) → (A → C) → A → D
lift op f g = λ x → op (f x) (g x)

on : {A B C : Set} → (B → B → C) → (A → B) → A → A → C
on op f x y = op (f x) (f y)

compose2 : {A B C D : Set} → (C → D) → (A → B → C) → A → B → D
compose2 f g = λ x y → f (g x y)

id-id : {A : Set} → A → A
id-id = id id

const-id : {A B : Set} → B → A → A
const-id = const id

compose-id-id :  {A : Set} → A → A
compose-id-id = compose id id


{-

Now please feel free to mess around and experiment.

If you need some inspiration, here are some open ended exercises:

  Enter in an incorrect solution, i.e. "const x y = y" and look at what kinds of errors you can get.

  Move an implicit parameter (like {B : Set}) to different positions within a function type. When does this work? When does this not work?

  Introduce too many variables in a lambda or function definition. Wrongly introduce an implicit variable. What happens? Why?

  Define a function in terms of itself. What happens? Why might this be?

  Make up some logical propositions and see if you can or can't prove them. If you can't prove them, can you show that they imply an absurdity?


At the end of every file, you will find challenge exercises. These are purely for fun and you are not at all expected to complete them.

It will likely help to come back to them after completing later chapters and gaining more intuition and experience with Agda.

-}


-- Don't worry if Agda is highlighting parts of the definition here
-- It simply means that Agda hasn't been able to infer the implicit parameters given the (empty) type signature
-- The solution here should use all 5 type variables
compose2-[const-apply]-flip : {A B C D E : Set} → (A → B → C) → B → (D → E) → D → E
compose2-[const-apply]-flip = compose2 (const apply) flip

-- "((P → Q) → P) → P" is true. Strangely we can't prove it in Agda, but we *can* show that it implying an absurdity implies an absurdity.
-- We will explore this phenomenon more in later chapters
[[[[p→q]→p]→p]→absurd]→absurd : {P Q : Set} → ((((P → Q) → P) → P) → {A : Set} → A) → {A : Set} → A
[[[[p→q]→p]→p]→absurd]→absurd = λ [[[p→q]→p]→p]→absurd → [[[p→q]→p]→p]→absurd (λ [p→q]→p → [p→q]→p (λ p → [[[p→q]→p]→p]→absurd λ _ → p))