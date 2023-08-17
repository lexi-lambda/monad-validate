# 1.2.0.2 [not yet released]

- Added support for mtl 2.3.

# 1.2.0.1 [2022-07-05]

- Added support for GHC 9.0 and 9.2.

# 1.2.0.0 [2019-08-09]

- Added the `exceptToValidate`, `exceptToValidateWith`, `validateToError`, and `validateToErrorWith` functions for converting between different error-raising monads.
- Removed the `DefaultSignatures`-based default methods for `MonadValidate` in favor of a `WrappedMonadTrans` newtype available from `Control.Monad.Validate.Class` that can be used to derive instances using `DerivingVia`.
- Added a default implementation of `dispute` in terms of `refute` and `tolerate` and added their equivalence as a law for `MonadValidate`.

# 1.1.0.0 [2019-08-05]

- Added the `tolerate` method to `MonadValidate`, which allows relaxing validation errors from fatal to nonfatal.
- Added the `embedValidateT` and `mapErrors` functions, which can be used together to locally alter the type of validation errors in `ValidateT` computations.
- Removed the `MonadValidate` instance for `ContT`, which is no longer possible to implement due to the addition of `tolerate`.

# 1.0.0.0 [2019-08-04]

- Initial release.
