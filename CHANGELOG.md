# 1.1.0.0 [unreleased]

- Added the `tolerate` method to `MonadValidate`, which allows relaxing validation errors from fatal to nonfatal.
- Removed the `MonadValidate` instance for `ContT`, which is no longer possible to implement due to the addition of `tolerate`.

# 1.0.0.0 [2019-08-04]

- Initial release.
