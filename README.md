# monad-validate

A Haskell library providing the `ValidateT` monad transformer, designed for writing data validations that provide high-quality error reporting without much effort. `ValidateT` automatically exploits the data dependencies of your program—as encoded implicitly in uses of `fmap`, `<*>`, and `>>=`—to report as many errors as possible upon failure instead of completely aborting at the first one.

See [the documentation on Hackage][hackage] for more information and examples.

[hackage]: https://hackage.haskell.org/package/monad-validate
