# 1.1.0.0 [2019-08-05]

- Added the `tolerate` method to `MonadValidate`, which allows relaxing validation errors from fatal to nonfatal.
- Added the `embedValidateT` and `mapErrors` functions, which can be used together to locally alter the type of validation errors in `ValidateT` computations.
- Removed the `MonadValidate` instance for `ContT`, which is no longer possible to implement due to the addition of `tolerate`.

# 1.0.0.0 [2019-08-04]

- Initial release.
