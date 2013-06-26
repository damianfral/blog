What's wrong with you, JavaScript?
==================================

Prototypal Object Orientation
-----------------------------

Class-based object orientation is more common. **JavaScript is the only
mainstream language that has prototypes**. I don't want to say which one
is better, just that I feel more confortable with classes. JavaScript
prototypes can emulate class systems. In fact, there are many libraries
for that.

Modules and Var declaration
---------------------------

**JavaScript has no formal contruct for namespaces**. Module pattern
(passing the namespace as an argument to an anonymous self-invoking
function) is really helpful. Lexical scope and closures are awesome,
which allows the module pattern, but something as important as
namespaces must be part of the language, don't you think?

Another weird behaviour is related with the \`var\` keyword. If you
declare a variable with \`var\`, it will belong to the local scope. If
you don't, its scope will be global with overwritting problems and
performance penalty (the interpreter will look up the scope chain to
find the global variable).

Syntax
------

Event driven code is really common in JavaScript. Usually, if you don't
use promises or futures, you define a callback that is passed as
argument to a function that binds that function to an event, like DOM
events in the browser or EventEmitter events in Nodejs. So, when you
have a callback that binds another callback to another event that binds
another callback to... and enclosed by the module pattern, you will see
something like this:

I prefer identation over braces and parenthesis, so the above code looks
pretty ugly for me.

Dynamic typing
--------------

Delaying type-checking to runtime is nice for fast-prototyping and data
intensive applications. Not so many years ago, JavaScript was used as a
toy language to add some animations, form validations and simple stuffs.
AJAX changed it all. JavaScript has been growing up and nowadays it is
used in bigger apps (databases, game engines, communications apps,
multimedia apps..) that may have a high cyclomatic complexity. A stupid
unnoticed error misnaming a variable could slap your face in the worse
moment (*demo effect*).

Static typing is not the holy grail, but it helps. Haskell guys, as
[@EleDiaz](https://twitter.com/EleDiaz777) , know that if Haskell code
compiles, it is almost certain that it will work as expected.

And some web applications need performance. Doing some tasks in the
compilation time[^1] (AOT), can give us, for example, a higher FPS value
in our game.

Weak typing
-----------

Coercion plus dynamic typing results on a ***debugging hell***. Many
JavaScript programmers, I guess, have notice that debugging JavaScript
code is not easy. Many errors are hidden by implicit conversions. It
might speed up development, but it slows down debugging by a higher
factor.

Surprise behaviour
------------------

JavaScript is a joker. Let's see:

Despite of the fact that ES 6 has some new proposals to minify or solve
these and other *problems*, there is another path to avoid them: use
another programming language that targets JavaScript.

There are some new languages that try to hide the bad parts adding a
syntax sugar layer keeping the JavaScript semantics. Other languages
have a completely new syntax and semantics. And there is another option,
compilers for *old* languages that generate JavaScript code.

[^1]: Generally, JavaScript interpreters are JIT compiler, therefore
    there is a compilation time.
