hjsmin
======

[![Build Status](https://secure.travis-ci.org/erikd/hjsmin.png?branch=master)](http://travis-ci.org/erikd/hjsmin)

Haskell implementation of a javascript minifier

It is intended to be used in conjunction with Hamlet, part of Yesod.

As such, much of the structure of the package is shamelessly copied from Hamlet.

See http://github.com/snoyberg/hamlet


How to build
------------

Library:

cabal clean && cabal configure && cabal build

Tests:

    cabal clean && cabal configure --enable-tests && cabal build

Running the tests

    dist/build/test-hjsmin/test-hjsmin

Changes
-------

0.1.5.1 - fix if/else/if minify issue

0.1.5.0 - fix tests to work with language-javascript >= 0.5.14

0.1.4.7 - remove upper bounds in cabal file

0.1.4.6 - relax upper bound in optparse-applicative

0.1.4.5 - relax upper bound in text to support 1.1
        - introduce CLI wrapper for minifying files from the
          commandline, courtesy of @CodeBlock

0.1.4.4 - relax upper bound in text to support 1.0

0.1.4.3 - make sure all missing cases are covered

0.1.4.2 - minify octal literals too

0.1.4.1 - Bump upper bound for containers to < 0.6 for the tests as well as the library

0.1.4 - Include test assets in cabal to allow cabal test to pass. Courtesy of @snoyberg

0.1.3 - Update version ranges for GHC 7.6.1, courtesy of @mietek

0.1.2 - More general fix to the space after 'new' keyword, for issue #8 & #9

0.1.1 - Fixed problem with missing space after 'new' keyword, in issue #8.

0.1.0 - Major update to work with language-javascript 0.5.1. All changes should be internal.
        Update of build process to make use of Cabal testing support, and Travis CI.

0.0.15 - Fix GHC 7.4.1 compile compatibility. Patch accepted from github.com/luite

0.0.14 - Allow unicode characters in comments

0.0.13 - Error in parsing numbers with zeros before decimal point

0.0.12 - Worked in Michael Snoyman's fix for unicode output

0.0.11 - Worked in language-javascript 0.4.*, with source locations in the AST
         Worked in processing of property get/set in object literals
0.0.10 - Removed attoparsec dependency and historical Parse/Token


