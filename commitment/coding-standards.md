Hi! If you are reading this, you are interested in getting gud at FP (functional programming)

Here are some of the dogmas behind FP:

1: write as little code as possible (granular functions)
2: maintain data purity
3: focus on how data changes rather then state management

Here are some of the reasons we do this:

- reduces the change of state-related / runtime bugs (server crashing)
- reduces meteor compilation time by having to sift through less code
- creates easily refactorable components so minimal refactoring can create large changes
- reduces the need to error manage: if you use FP well all your errors will automatically propagate upwards!

You might want to look through the Haskell API (even if you don't understand Haskell)
and look at how the code is structured. Haskell is a purely FP language, that does not offer
any alternatives. For this reason, many optimisations can be made when running a
Haskell program, which makes it as fast as any old well written C code (blazingly fast)!

You might notice that the code is well structured, and many function declarations are just one line of code each.
This is what we call a granular function, as it is only responsible for doing a small amount of work.
The whole point of FP is that you can build an architecture of granular functions, and each time you
made a new function those functions can do increasingly more and more powerful things while remaining
small and easily changeable. For a good typescript example look at server/helper_functions.

If you look at the threading file, you will see many functions with "submit" in them. Just from
making the "submit" functions, only three functions later in a single line I can send two functions
through a fully atomically optimised async pipeline to process one of the functions on one threadpool
and another function on another thread pool, ensuring load balancing occurs between parsing operations.
This is what makes functional programming incredibly powerful with so little code!

One reason Haskell allows you to leverage granular functions so well is by a concept called "currying":
https://en.wikipedia.org/wiki/Currying

You can read the top summary on the Wiki page for what they are. This allows functions to take other
functions as arguments, and allows functions to be saved into variables to be reused, and as such
it makes using functions very dynamic, intrinsic and small. It also allows you to avoid defining the
bounds by which the data is used, and only focus on how the functions are applied, avoiding any state management!
Curring is a good technique to employ when you want to reuse a captured variable. For example:

const executeLocally = execCommmand(local_path)

We see that the variable executeLocally is actually a function, which takes a Command as an argument.
This means that if we wanted to run multiple commands in that directory, you can do so:

const r1 = executeLocally(c1)
const r2 = executeLocally(c2)

which looks a lot better than:

const r1 = execCommmand(local_path, c1)
const r2 = execCommmand(local_path, c2)

It makes things readable when you employ good variable naming strategies and reduces the length of your code.
Its a nice to have when trying to find the error you made 2 months ago, or when peer programming when
you might not fully understand what the code is doing.

By default, no variable in Haskell is mutable, which is to say once it is instantiated the variable can
no longer be changed. This is called data "purity", as it means data will never get muddied with any other value,
which reduces the chance of your code exploding our app.

Wierd bugs could arise from this code:

let a: number | undefined = undefined

switch (text) {
case "yeet" -> number = 69
case "yo mama" -> number = 420
}
// a could still be undefined here :/

or you could do something like this:

const a: number = text === "yeet" ? 69 : 420

this ensures a is properly defined, and no wierd runtime bugs occur as a result of delegating the value of the variable
to a later process. also, look at how many lines you saved!

Hopefully you have learned a little bit more about the infamous FP! Try using these principles as best
as you can in this Repo's code base, because it makes the code a lot easier to work on later,
and it avoids any server crashes :D. Happy Coding!
