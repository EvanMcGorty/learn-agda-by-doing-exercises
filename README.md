# Learn Agda by Doing Exercises

See [Why-You-Should-Learn-Agda.md](Why-You-Should-Learn-Agda.md)

This Agda tutorial is a series of chapters, in the form of interactive .agda files, which thoroughly motivate and teach dependently typed programming and related topics. It is intended first and foremost to be enjoyable, accompanying every explanation with fun, interesting exercises designed to incrementally build intuition, without making conceptual leaps. A primary goal of this tutorial is to significantly lower the entry barrier to the world of dependent types by clearly and exhaustively covering all the important concepts which might otherwise take too much time and effort to research and make sense out of. In this sense, this tutorial is like a textbook, and chapters are organized so that they may be easily referenced, with the [Table-of-Contents.md](Table-of-Contents.md) serving as a precise index of taught concepts.

The only prerequisite to this tutorial is a solid level of general comfort with programming, especially type-generic programming. Experience with functional programming is not at all necessary, but could be advantageous.

Chapters may be completed independently, though they generally build off of content from previous chapters. Definitions mostly use naming conventions from the Agda stdlib while providing ascii synonyms for users who don't want to type unicode symbols. This quietly helps condition the reader to have an easier time navigating and understanding the Agda stdlib, should they eventually want to use it. Each chapter imports definitions from the provided solutions (in the Solutions directory) to previous chapters, so exercises do not depend on the reader's solutions (in the Exercises directory).

To get started, [install Agda](How-To-Install-Agda.md) (I promise it's worth it) and clone this repo. Then open [Exercises/Ch0.agda](Exercises/Ch0.agda) (or whatever piques your interest, if you have prior experience) and start reading.

Explanations may contain lines up to 150 characters long, so some readers may wish to turn on word-wrap in their editor.

This tutorial should work for Agda versions 2.6.3 and later.

## Other Resources

This tutorial aims to be more or less exactly what I wish existed when I started learning Agda. Topics are stretched out more and are explored much more clearly and exhaustively than they are any other Agda tutorial which I am aware of. Nonetheless, learning from a single resource is rarely ideal. The amount of new knowledge you need to absorb tends to ramp up very quickly when following any Agda tutorial and, consequently, you may hit a dead end, at which point things become too difficult and confusing to progress further. I therefore highly recommending learning from multiple sources at once, as they will all re-explain various important concepts in different ways and touch on slightly different topics. This will also allow you to get as far as you can with each resource, and then move on to the next. By the time you circle back around to a resource again, you will understand more and be able to make it further. For this purpose I recommend the following resources. Start at the top and work your way down as you see fit. If one resource isn't working then try another one, and maybe come back to the first one later.

### Agda From Nothing Follow-Along Introductory Lectures

[Github repo](https://github.com/scott-fleischman/agda-from-nothing)

[Part 1 on Youtube](https://www.youtube.com/watch?v=-i-QQ36Nfsk)

[Part 2 on Youtube](https://www.youtube.com/watch?v=XprGyVWXwks)

- Short lecture series recording
- For a (more) general audience
- Stays relatively surface-level

### Connor McBride's Lecture Series CS410 2017

[Github repo with exercises and lecture notes](https://github.com/pigworker/CS410-17)

[Youtube playlist Lecture 1](https://www.youtube.com/watch?v=O4oczQry9Jw&list=PLqggUNm8QSqmeTg5n37oxBif-PInUfTJ2&t=500s)

- In-depth university course recording
- Great exercises :^)
- Sharp learning curve

### Agda Tutorial from ELTE Budapest

[Agda Tutorial](https://people.inf.elte.hu/divip/AgdaTutorial/Index.html)

- Programmer's tutorial to Agda
- Open ended exercises :^)
- Easy to navigate

### Official Agda Documentation

[Index](https://agda.readthedocs.io/en/latest/language/index.html)

[Interactive Mode Commands](https://agda.readthedocs.io/en/latest/tools/emacs-mode.html#keybindings)

- Exhaustive documentation of all of Agda
- Definitely not intended to be used as a tutorial
- Still can be very helpful and informative about specific features

### Zulip Chatroom

[Zulip Agda](https://agda.zulipchat.com/)

- Most popular (to my knowledge) Agda-related chat/forum
- Many friendly and enthusiastic people ready to help and answer questions
- You can learn a lot by reading answers to old questions

### Codewars Exercises

[Codewars.com](https://www.codewars.com/)

[Codewars Agda Docs](https://docs.codewars.com/languages/agda)

- Still "in beta" though completely usable as far as I can tell
- Plenty of good and fun challenges about various topics
- Uses an old version of stdlib :(

### An introduction to Agda targeted at Haskell programmers

[Programming and Proving in Agda](https://github.com/jespercockx/agda-lecture-notes/blob/master/agda.pdf)

- Simple and well written intro
- Doesn't go super deep
- Exercises are good but few

### An introduction to programming language theory in Agda with exercises

[Programming Language Foundations in Agda](https://plfa.github.io/Preface/)

[Exercises](https://github.com/plfa/plfa.github.io/tree/dev/courses/TSPL/2022)

- Generally challenging content
- Lots of great exercises :^)
- Introduces the Agda stdlib

### A video introduction series on Agda and Type Theory

[Introductory Lectures on Type Theory playlist first lecture](https://www.youtube.com/watch?v=Y7blCeETJo8&list=PL3XL6suc7Hp70kLZVUImSDYXd4GE_E8Ys)

- Goes slowly and clearly through some underlying theory of Agda
- Examines various topics in logic and type theory
- Content is further away from practical programming

### An interactive introduction to Agda and Homotopy Type Theory

[The HoTT Game](https://thehottgameguide.readthedocs.io/en/latest/index.html)

- Dives into homotopy type theory in addition to plain type-theory/logic
- Learning by doing exercises :^)
- Aimed at Mathematicians

### A series of lectures on (Homotopy) Type Theory and (Cubical) Agda with exercises

[Type Theory Lecture 1](https://www.youtube.com/watch?v=HvYYCHMeM-8&list=PLtIZ5qxwSNnzpNqfXzJjlHI9yCAzRzKtx&index=1)

[Type Theory Exercises](https://github.com/martinescardo/HoTTEST-Summer-School/tree/main/HoTT/Worksheets)

[Agda Lecture 1](https://www.youtube.com/watch?v=fJxWLQaaCX4&list=PLtIZ5qxwSNnzpNqfXzJjlHI9yCAzRzKtx&index=3)

[Exercises for Agda lectures 1-3, Vanilla Agda](https://github.com/martinescardo/HoTTEST-Summer-School/tree/main/Agda/Exercises)

[Exercises for Agda lectures 4-6, Higher Inductive Types](https://github.com/martinescardo/HoTTEST-Summer-School/tree/main/Agda/HITs)

[Exercises for Agda lectures 7-9, Cubical Agda](https://github.com/martinescardo/HoTTEST-Summer-School/tree/main/Agda/Cubical)

- Large series with lectures both on pure theory, and on using Agda
- Lots of great exercises :^)
- Content can sometimes be difficult to follow

## The Best Resource

And finally, the most important resource (in my opinion), is to just go screw around in Agda. Try to formalize random things that interest you, play around, and have FUN.

Good luck on your Agda journey!