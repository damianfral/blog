---
title: Simple Monads Example (CoffeeScript)
tags: javascript, coffeescript, coffee-script, monad, monads, maybe monad, simple monad example, simple monad, functional programming
---

This article is based on `Why use monads`_.

.. _`Why use monads`: http://www.intensivesystems.net/tutorials/why_monads.html

Suppose you have some functions that take a number and answer another number or *null*:

.. code:: Coffeescript

    inc = (x) ->
        if x isnt 6
            x + 1
        else
            null

    double = (x) ->
        if x % 2
            x * 2
        else
            null

    dec = (x) ->
        if x isnt 3
            x - 1
        else
            null


Now, you want to compose a new function chaining previous functions. Something like this:

.. code:: Coffeescript

    all = (x) ->
        dec(double(inc(x)))


This is not the most elegant way of chaining functions, so you write a *generic chain* function:

.. code:: Coffeescript

    chain = (funcs...) ->
        (arg) ->
            for func in funcs
                arg = func arg
            return arg

    all = chain dec, double, inc
    all 3 # Wrong result: 3. Expected result: null

In spite of being a better path, you still have to deal with null values or otherwise we will see things like last line of the above code (in JavaScript *null* plus 1 is 1). You could change previous chaining function in order to be specific for this problem or create new all function that stops running and returns null when a *chained* function returns *null*:

.. code:: Coffeescript

    all = (x) ->
        return null if tmp is null
        tmp = inc x
        return null if tmp is null
        tmp = double tmp
        return null if tmp is null
        tmp = dec tmp
        return tmp

There is a clear pattern there; these functions are monadic functions under the **maybe monad**, we can combine them like this [#]_:

.. code:: Coffeescript

    all = doMonad MaybeMonad, inc, double, dec
    all null # null
    all 3    # null
    all 8    # 17

Other languages like Haskell allow this kind of composition without effort. In JavaScript (or CoffeeScript) we have to work a little more to get it:

.. code:: Coffeescript

    None       = null
    MaybeMonad =
        mReturn: (value) ->
            if value in [undefined, null, NaN]
                return None
            return value

        mBind: (value, f) ->
            return None if value is None
            return f value



    doMonad = (monad, funcs...) ->
        (result) ->
            iterator = (i = 0) ->
                if i is funcs.length
                    return monad.mReturn result
                else
                    result   = funcs[i] result
                    return monad.mBind result, -> iterator i + 1

            return monad.mBind (monad.mReturn result), -> iterator 0


What the hell do we have here? It easier than it seems. There is a *None* variable, a *MaybeMonad* associative array and a doMonad function. The MaybeMonad has 2 functions.

- **mReturn** takes a value and transform it (or maybe not).

- **mBind** takes a value (it will be always returned by mReturn) and a function and do some computations to call that function (or maybe not).

The doMonad functions takes a monad and a bunch of functions (funcs) and returns a new function. This function will receive an argument (result) and it will iterate over funcs to call them with the value returned for the previous function as argument except for the first function (it has not previous function) that receives the same argument, called result, as the wrapper function. The special thing here is that these iterative execution of functions are *proxied* with monad.mBind. In the case of MaybeMonad, mBind decides if the next function will be executed or not.

The point is doMonad is generic and you can write your own monads to composing functions with your own pattern. Let's see the List monad:

.. code:: Coffeescript

    flatten = (value) ->
        if Array.isArray value
            output = []
            value.map (elem) ->
                if Array.isArray elem
                    output = output.concat flatten elem
                else
                    output.push elem
            return output
        else
            return [value]


In this case, *mBind* doesn't stops the chained executions, but just flats the returned array received as argument. We can get this:

.. code:: Coffeescript

    ListMonad =
        mBind: (list, f) ->
            output = list.map f
            return flatten output

        mReturn: (value) ->
            return [value]


    replicate = (n) ->
        (v) -> [0...n].map -> v

    generation = (value) -> (replicate 3) value

    f = doMonad ListMonad, generation, generation
    f ["No God! Please no!"]

    # [ 'No God! Please no!',
    #   'No God! Please no!',
    #   'No God! Please no!',
    #   'No God! Please no!',
    #   'No God! Please no!',
    #   'No God! Please no!',
    #   'No God! Please no!',
    #   'No God! Please no!',
    #   'No God! Please no!' ]

.. [#] Copy-pasted sentence.
