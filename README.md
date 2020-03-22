# Scalac Survival Guide

Tips and Tricks for working with Scala reflection, macros, and compiler internals.

## Info

As a programmer, tinkering with the implementation of the very language
you work with (and think in) is a uniquely satisfying endeavour. Scala
offers lots of avenues to try this: you can author compiler plugins to
perform static analysis, use macros to generate boilerplate, or modify
the compiler to fix bugs or implement your own pet feature. More
generally, thinking about the meta-aspects of the language invariably
lead to a fresh perspective and deeper understanding that benefit your
regular coding, too.

But all of these tasks can at times seem daunting, infuriating, or out
of reach. This guide will issue you with the map, compass, flashlight,
and roll of duct tape that you need to embark on this journey. It is
drawn from the tools and techniques that I use on a daily basis when
I'm working on the compiler.

## Further Learning

The [Reflection](http://docs.scala-lang.org/overviews/reflection/overview.html)
and [Macro](http://docs.scala-lang.org/overviews/macros/usecases.html)
guides include a handy overview of the Trees / Symbols / Types.
