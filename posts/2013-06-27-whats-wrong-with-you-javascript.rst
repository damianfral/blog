----
title: What's wrong with you, JavaScript?
----

.. We can't escape from JavaScript. It's the language of the web.

Prototypal Object Orientation
.............................

Class-based object orientation is more common. **JavaScript is the only mainstream language that has prototypes**. I don't want to say which one is better, just that I feel more confortable with classes. JavaScript prototypes can emulate class systems. In fact, there are many libraries for that.


Modules and Var declaration
...........................

**JavaScript has no formal construct for namespaces**. Module pattern (passing the namespace as an argument to an anonymous self-invoking function) is really helpful. Lexical scope and closures are awesome, which allows the module pattern, but something as important as namespaces must be part of the language, don't you think?
    
Another weird behaviour is related with the `var` keyword. If you declare a variable with `var`, it will belong to the local scope. If you don't, its scope will be global with overwritting problems and performance penalty (the interpreter will look up the scope chain to find the global variable).

Syntax
......

Event driven code is really common in JavaScript. Usually, if you don't use promises or futures, you define a callback that is passed as argument to a function that binds that function to an event, like DOM events in the browser or EventEmitter events in Nodejs. So, when you have a callback that binds another callback to another event that binds another callback to... and enclosed by the module pattern, you will see something like this:

.. code:: Javascript

    (function() 
    {
        $("#my-button1").on("click", function(msg1) 
        {
            alert(msg1);
            $("#my-button2").on("click", function(msg2)
            {
                alert(msg2);
                $("#my-button3").on("click", function(msg3) 
                {
                    alert(msg3);
                });
            });
        });
    }).call(this);

I prefer indentation over braces and parenthesis, so the above code looks pretty ugly for me.


Dynamic typing and late binding
...............................

Delaying type-checking to runtime is nice for fast-prototyping and data intensive applications (generating types and behaviours based on runtime data). Not so many years ago, JavaScript was used as a toy language to add some animations, form validations and simple stuffs. AJAX changed it all. JavaScript has been growing up and nowadays it is used in bigger apps (databases, game engines, communications apps, multimedia apps..) that may have a high cyclomatic complexity. Since JavaScript just checks types and binds names in runtime, a stupid unnoticed                                                                                                                  error misnaming a variable or a method could slap your face in the worst moment (*demo effect*).

Static typing is not the holy grail, but it helps. Haskell guys, as `@EleDiaz`_ , know that if Haskell code compiles, it is almost certain that it will work as expected.

.. _`@EleDiaz`: https://twitter.com/EleDiaz777

And some web applications need performance. Doing some tasks in the compilation time[1]_  (AOT), can give us, for example, a higher FPS value in our game.

.. [1] Generally, JavaScript interpreters are JIT compiler, therefore there is a *classical* compilation time.

Weak typing 
...........

Coercion plus dynamic typing results on a ***debugging hell***. Many JavaScript programmers, I guess, have noticed that debugging JavaScript code is not easy. Many errors are hidden by implicit conversions. It might speed up development, but it slows down debugging by a higher factor.


Surprise behaviour
..................

JavaScript is a joker. Let's see:

.. code:: Javascript

    var a = (7, 5); // -> a = 5
    typeof({} + []) // -> "string"
    typeof([] + {}) // -> "number"

    "1" >= -Infinity // true
    "A" >= -Infinity // false
    "A" < -Infinity  // false

Despite of the fact that ES 6 has some new proposals to minify or solve these and other *problems*, there is another path to avoid them: use another programming language that targets JavaScript. 

There are some new languages that try to hide the bad parts adding a syntax sugar layer keeping the JavaScript semantics. Other languages have a completely new syntax and semantics. And there is another option, compilers for *old* languages that generate JavaScript code.

.. Let's see some examples of these languages.


.. New languages targetting JavaScript
.. +++++++++++++++++++++++++++++++++++

.. CoffeeScript
.. ............

.. .. pull-quote::

..  CoffeeScript is a little language that compiles into JavaScript. Underneath that awkward Java-esque patina, JavaScript has always had a gorgeous heart. CoffeeScript is an attempt to expose the good parts of JavaScript in a simple way.

..  -- Jeremy Askenash

.. The CoffeeScript's motto is *It's just JavaScript*. And it's true. CoffeeScript preserve JavaScript semantic and adds some syntax sugar that remains to Ruby, Python and Haskell.

.. Dart
.. ....


.. TypeScript
.. ..........


.. Roy
.. ...


.. Wisp
.. ....


.. LLJS
.. ....


.. ASMjs
.. .....


.. Alternatives - Compile "old" languages to JavaScript
.. ++++++++++++++++++++++++++++++++++++++++++++++++++++

.. Emscripten (C++)
.. ................


.. Fay (Haskell)
.. .............

.. Fay is sub-language of Haskell that compile to javascript, which preserve characteristics of functional language, that help us resolve the problems of javascript of way more funtional and pure.

.. A few reasons for what you must have choice Fay, inheriting the characteristics of haskell, that are the follow:

.. - Purely-Funtional (Interact with exterior only with the monads)

.. - Curryfing (Partial aplication of funtions)

.. - Strongly type (Reduce bugs)

.. - Type System

.. Go with an example:

.. .. code:: Haskell
..  :tab-width: 2

..  module Console (main) where

..  import Prelude
..  import FFI

..  main = putStrLn (showInt (fib 10))

..  fib :: Int -> Int
..  fib 0 = 0
..  fib 1 = 1
..  fib n = fib (n - 1) + fib (n - 2)

..  showInt :: Int -> String
..  showInt = ffi "%1+''"

.. Ohhh, it is beautiful, but how i can translate to javascript? Very easy:
    
.. .. code:: Shell

..  $ fay fib.hs

.. This generate fib.js with the necesary for compile in node. But this isn't all, there are more:

.. - The file is very big, for example the above code in js take 683 lines.
.. - Big line + 51000 chars


.. .. _Fay: https://github.com/faylang/fay/wiki


.. UHC-JS(The Utrecht Haskell Compiler JavaScript Backend)
.. .......................................................




.. ClojureScript
.. .............



.. Conclusion
.. ----------
