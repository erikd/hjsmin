hjsmin
======

Haskell implementation of a javascript minifier

It is intended to be used in conjunction with Hamlet, part of Yesod.

As such, much of the structure of the package is shamelessly copied from Hamlet.

See http://github.com/snoyberg/hamlet


How to build
------------

Library:

cabal clean && cabal configure && cabal build

Tests:

cabal clean && cabal configure -fbuildtests && cabal build

Running the tests

./dist/build/runtests/runtests





