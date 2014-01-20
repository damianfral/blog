---
title: Alternatives to JavaScript (I)
---

JavaScript is the most ubiquitous computing runtime in the world. Web browsers are, by far, the most common host environments for JavaScript, probably followed by server platforms (see `Node.js`_). But JavaScript engines are embedded in many other tools; from microcontrollers, like Espruino, to graphical shells like GNOME Shell; from PDF documents to graphic editing programs, like Adobe Photoshop.

As I wrote in my `first post`_ on this blog, JavaScript has some design problems and, in my opinion, these problems make large application development really hard.

So, as it's pointed at http://www.haskell.org/haskellwiki/The_JavaScript_Problem, JavaScript sucks but we need JavaScript. As a result various alternative languages compiling to JavaScript and compilers for old language that generate JavaScript have emerged.

To see a a good list of languages and compilers that target JavaScript, visit https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS. There are more than 200!

The process of *running another language* in a JavaScript interpreter is simplified if the developer tools for that platform support `source maps`_, which maps the generated JavaScript to the original code. This way, you can debug, optimize, test... using the original code. This doesn't mean you can forget or avoid learning JavaScript; in the end, JavaScript engines work with JavaScript.

.. _`first post`: 2013-06-27-whats-wrong-with-you-javascript.html
.. _`source maps`: http://www.html5rocks.com/en/tutorials/developertools/sourcemaps/?redirect_from_locale=es


New languages that target JavaScript
------------------------------------

I put in this section three languages with different *tastes* that have been developed to generate JavaScript code, licensed under open-source licenses and that I would consider to use.

These new languages are pretty different, CoffeeScript might remind you Python and Ruby, TypeScript is really influenced by Java, while PureScript belongs to the Haskell school. All they produce small (ready for client-side development) and readable code, so, don't be afraid and give them a try to see if they fit your needs; if you want to return to JavaScript, just use the generated JavaScript.

CoffeeScript
++++++++++++

The most popular alternative to JavaScript is CoffeeScript_, in fact, At Quobis_, we use CoffeeScript instead of JavaScript. His creator, `Jeremy Ashkenas`_ (also the creator of `Backbone.js`_ and `Underscore.js`_) says:

	CoffeeScript is a little language that compiles into JavaScript. Underneath that awkward Java-esque patina, JavaScript has always had a gorgeous heart. CoffeeScript is an attempt to expose the good parts of JavaScript in a simple way.

CoffeeScript is just a syntactic sugar layer over JavaScript inspired, mostly, by Python and Ruby. Hence, it's just another syntax for writting JavaScript code. It avoids some JavaScript pitfalls and adds some features like *sort of* object-oriented programming via classes, implicit returns, list comprehensions or postfix operators for loops and conditionals.

Its main advantage is, at the same time, its bigger drawback: it's just a saner way to write JavaScript code, but it's JavaScript. Actually, the golden rule of CoffeeScript is *It's just JavaScript*.

It is worth pointing out there a good bunch of CoffeeScript forks and dialects with different goals like Coco_, that aims to be more radical, or IcedCoffeeScript_, which is a superset of CoffeeScript that adds some keywords to deal with asynchronows control flow.


TypeScript
++++++++++

TypeScript_ is a Microsoft project that adds structural typing on top of JavaScript:

	TypeScript is a language for application-scale JavaScript development.
	TypeScript is a typed superset of JavaScript that compiles to plain JavaScript.

You can think on TypeScript as JavaScript plus optional static types and class-based object-oriented programming; so, TypeScript provides support for classes, properties, inheritance, interfaces and namespaces. By default, TypeScript delegates to dynamic typing (type ``any``) when it can't find the type out. So, valid JavaScript code is valid TypeScript code too.

The TypeScript compiler has a flag, ``--noImplicitAny``, which fires a warning on untyped expressions and declarations. There is `a repository in github <https://github.com/borisyankov/DefinitelyTyped>`_ where you can find TypeScript type definitons for more thab 300 JavaScript libraries and APIs.


PureScript
++++++++++

PureScript_ is a `Phil Freeman`_'s project. PureScript is a rewording of JavaScript with a strongly type system in the style of Haskell type system. In the words of its creator:

	I was looking for a simple functional language which would compile to JavaScript and have the following characteristics:

	- Generates simple readable Javascript
	- Provides the ability to compile to tight loops if necessary
	- Reasonable type system
	- Ideally, written in a programming language I would enjoy working in
	- Provides a simple interface to existing Javascript code

	PureScript is not designed to be a general-purpose programming language. The primary use case is as a generator for purely-functional core libraries, with the main application code written in another language.

A look to PureScript code shows is clearly inspired by Haskell. As Haskell, it provides algebraic datatypes, homogeneous arrays, polimorphic types, type inference, typeclasses, pattern matching... If you are a Haskeller, you likely want to try PureScript.

The main problem with PureScript is, contrary to what happens to TypeScript, the absent of wrappers for most common JavaScript libraries. It provides a simple Foreign Function Interface that allows you to write wrappers by yourself. This language is neither popular enought nor supported by a big company. I hope that PureScript will increase its community, so, this problem will be a matter of time.

.. _`Node.js`: http://nodejs.org/
.. _CoffeeScript: http://coffeescript.org/
.. _Coco: https://github.com/satyr/coco
.. _Quobis: https://twitter.com/Quobis
.. _`Jeremy Ashkenas`: https://twitter.com/jashkenas
.. _`Phil Freeman`: https://twitter.com/paf31
.. _`Backbone.js`: http://backbonejs.org/
.. _`Underscore.js`: http://underscorejs.org/
.. _TypeScript: http://www.typescriptlang.org/
.. _PureScript: https://github.com/paf31/purescript