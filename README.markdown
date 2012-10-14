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

Changes
-------

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




