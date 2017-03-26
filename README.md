Caught in a Web of Functions
============================

## Abstract

Functional programming is a great tool for enabling programmers to
solve complex problems through the use of data and functions.
However, not all data and functions are created equal, and some are less
composable than others.

Web libraries in particular are guilty of being overly opinionated and
ending up as closed worlds of functionality. As programmers
we are complicit whenever we find ourselves asking:

"What web framework should I use"?

But why do we need to choose a single framework in the first place?
Why can't we compose functions from different libraries?
Do we really need 5 different ways to get/set a cookie?

This talk aims to deconstruct what we typically expect from a web
framework, and how we can achieve the same functionality with a series of
small, composable functions. This will be demonstrated using Haskell's WAI
library as a common building block, and contrasted to the
equivalent implementation in other modern Haskell web frameworks.

By the end of this talk web frameworks will be demystified and exposed for
what they should be - a collection of composable functions.
In addition the audience will have gained confidence in their
ability to recognise good and bad functions, and to better
evaluate the cost of the libraries they inhabit.


## Talk

http://blog.charleso.org/lambdajam-web-functions/


## Code

- [wai](charlotte-wai/)
- [scotty](charlotte-scotty/)
