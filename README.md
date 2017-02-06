monadic test
============

Library for testing Haskell code that uses `(Monad m) => m FooBar` pattern to return a value or fail with an error message.

At this point is more of experiment. Main.hs serves as an example of how it can be used.

Implements `fail` and `return` methods for its own data type, which then help capture the output.
