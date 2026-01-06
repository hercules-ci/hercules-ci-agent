
# Changelog

All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]


## [0.5.1.0] - 2026-01-06

- Nix support up to 2.33

## [0.5.0.0] - 2025-07-18

- `init` now installs synchronous signal handlers for you. You may remove your call to `installDefaultSigINTHandler`.

## [0.4.0.0] - 2025-05-05

- **REMOVE** `evalArgs`. This removes a dependency on the otherwise unused nix-cmd library. Use `valueFromExpressionString` and a library like `optparse-applicative` instead to create a nicer user experience.
- Nix 2.28 support

## [0.3.6.5] - 2024-11-15

- Nix 2.24 support
  This includes the newly separate `nix-flake` library as a dependency of `hercules-ci-cnix-expr`.

## [0.3.6.4] - 2024-06-12

Got rid of unnecessary data files, improving packaging.

## [0.3.6.3] - 2024-05-19

Solve build warnings.

## [0.3.6.2] - 2024-05-03

### Fixed

- Remove most uses of `c_str()`, of which at least one exhibited undefined behavior.

## [0.3.6.1] - 2024-02-12

### Added

- Nix support up to 2.20

## [0.3.6.0] - 2023-06-28

### Added

- `allowThreads`, `runInGcSafeThread`

- Nix 2.15 support

- Nix 2.16 support

## [0.3.5.1] - 2023-03-06

### Added

- Nix 2.14 support

- Nix 2.13 support

## [0.3.5.0] - 2022-12-29

### Added

- `initThread` for stack overflow handling

## [0.3.4.0] - 2022-11-15

### Added

- `::??`, `#??` for redundantly optional types: unset or null
- Schema module instances for `()` representing `null`
- `InvalidValue` constructor for `NixException`
- `.` and `?` type operators for field access and optional field access
- Schema module instances for `[a]` for Nix lists of `a`,
- Schema `Int64` (= `NixInt`) instance for Nix integers
- Schema `traverseArray` helper for traversing Nix lists (should have been renamed)


## [0.3.3.0] - 2022-06-21

### Added

- Nix 2.9 support


## [0.3.2.0] - 2022-05-17

### Added

- Nix 2.8 support

## [0.3.1.2] - 2022-04-08

### Fixed

- `getFlakeFromGit` now handles branch names correctly

## [0.3.1.1] - 2022-03-21

### Removed

- Clean up inline-c generated exports

### Fixed

- Add test data that was missing from the sdist tarball

## [0.3.1.0] - 2022-03-18

### Added

- `instance ToValue (PSObject t)`

## [0.3.0.0] - 2022-03-15

### Added

- `ToValue` and `ToRawValue` classes for converting Haskell values
   to Nix values.

- `Hercules.CNix.Expr.Schema` module for a typed interface between
   code in the Nix language and Haskell. This also includes
   provenance tracking, improving error messages while reducing
   error handling noise in the Haskell code.

- `addAllowedPath`, `addInternalAllowedPaths` for use with restricted mode.

### Changed

- Flakes are enabled during `init`

### Removed

- Nix 2.3 support

## [0.2.0.2] - 2022-03-09

### Added

- Nix 2.7 support

## [0.2.0.1] - 2022-03-07

### Added

- Improved conditional code support with `cabal-pkg-config-version-hook`

### Fixed

- Build with newer Nix versions 2.5, 2.6

## [0.2.0.0] - 2021-06-22

### Added

- nixUnstable compatibility with flag `nix-2_4`

## [0.1.0.0] - 2021-03-07

### Added

- First code based on Hercules CI Agent.


[0.5.1.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.5.1.0/hercules-ci-cnix-expr
[0.5.0.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.5.0.0/hercules-ci-cnix-expr
[0.4.0.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.4.0.0/hercules-ci-cnix-expr
[0.3.6.5]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.6.5/hercules-ci-cnix-expr
[0.3.6.4]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.6.4/hercules-ci-cnix-expr
[0.3.6.3]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.6.3/hercules-ci-cnix-expr
[0.3.6.2]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.6.2/hercules-ci-cnix-expr
[0.3.6.1]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.6.1/hercules-ci-cnix-expr
[0.3.6.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.6.0/hercules-ci-cnix-expr
[0.3.5.1]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.5.1/hercules-ci-cnix-expr
[0.3.5.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.5.0/hercules-ci-cnix-expr
[0.3.4.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.4.0/hercules-ci-cnix-expr
[0.3.3.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.3.0/hercules-ci-cnix-expr
[0.3.2.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.2.0/hercules-ci-cnix-expr
[0.3.1.2]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.1.2/hercules-ci-cnix-expr
[0.3.1.1]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.1.1/hercules-ci-cnix-expr
[0.3.1.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.1.0/hercules-ci-cnix-expr
[0.3.0.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.3.0.0/hercules-ci-cnix-expr
[0.2.0.2]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.2.0.2/hercules-ci-cnix-expr
[0.2.0.1]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.2.0.1/hercules-ci-cnix-expr
[0.2.0.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.2.0.0/hercules-ci-cnix-expr
[0.1.0.0]: https://github.com/hercules-ci/hercules-ci-agent/commits/hercules-ci-cnix-expr-0.1.0.0/hercules-ci-cnix-expr
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/commits/master/hercules-ci-cnix-expr
