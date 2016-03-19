hjsmin
======

[![Build Status](https://secure.travis-ci.org/erikd/hjsmin.png?branch=master)](http://travis-ci.org/erikd/hjsmin)

Haskell implementation of a command line javascript minifier.

The executable generated from this package simply does command line parsing
before handing the off the minification process to the [language-javascript]
package which also does the rendering.


How to build
------------

	cabal clean && cabal configure && cabal build

Tests
-----

There are currently no tests, because all the heavy lifting is done by
[language-javascript].


Reporting Bugs
--------------

Bugs like failing to parse certain chunks of Javascript or errors in the
minification process should be reported on the [language-javascript] issue
tracker.

Bugs about failure to handle command line paramters should be reported on the
[hjsmin] issue tracker.

[hjsmin]: https://github.com/erikd/hjsmin
[language-javascript]: https://github.com/erikd/language-javascript
