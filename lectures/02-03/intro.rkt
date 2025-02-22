#lang racket

#|

Today's lecture: what's in a name?

Macros use names in lots of ways...
 - As local temporaries
 - To refer to runtime support
 - New binding forms in terms of existing ones

If names are represented just as symbols, things go wrong.

But using syntax objects, things mostly just work: "Macro hygiene"
How can we understand what's going on?


In the presence of macro hygiene, what does it mean to compare names
for equality?
 - For a "duplicate binding" error
 - For keywords like `else`
 - For duplicate checking in `case`

|#
