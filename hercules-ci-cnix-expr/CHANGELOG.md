
# Changelog

All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## Unreleased

### Added

 - `getLocalFlake` using either flake-compat or Nix 2.4pre

 - `ToValue` and `ToRawValue` classes for converting Haskell values to Nix values.

 - `Hercules.CNix.Expr.Schema` module for a typed interface between code in the Nix language and Haskell. This also includes provenance tracking, improving error messages while reducing error handling noise in the Haskell code.

 - `addAllowedPath`, `addInternalAllowedPaths` for use with restricted mode.

### Changed

 - Flakes are enabled during `init` on Nix 2.4pre


## 0.2.0.1 - 2022-03-07

### Fixed

 - Build with newer Nix versions 2.5, 2.6

### Added

 - Improved conditional code support with `cabal-pkg-config-version-hook`

## 0.2.0.0 - 2021-06-22

### Added

 - nixUnstable compatibility with flag `nix-2_4`

## 0.1.0.0 - 2021-03-07

### Added

 - First code based on Hercules CI Agent.
