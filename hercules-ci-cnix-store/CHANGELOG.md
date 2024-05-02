
# Changelog

All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## 0.3.5.1 - 2024-05-03

### Fixed

 - Remove most uses of `c_str()`, of which at least one exhibited undefined behavior.

## 0.3.5.0 - 2024-02-12

### Added

 - `queryPathInfoFromClientCache`

 - Nix support 2.17 up to 2.20

 - A `both` function to combine `Verbosity`

## 0.3.4.0 - 2023-06-28

### Added

 - Nix 2.16 support

 - Nix 2.15 support

 - `getUseSQLiteWAL` low level function. (Use of Nix sqlite files is not recommended)

## 0.3.3.5 - 2023-03-06

### Added

 - Nix 2.14 support

 - Nix 2.13 support

## 0.3.3.4 - 2023-03-06

A dud.

## 0.3.3.3 - 2022-12-29

### Added

 - Nix 2.12 support

## 0.3.3.2 - 2022-11-15

### Added

 - Nix 2.11 support

## 0.3.3.1 - 2022-07-21

### Added

 - Nix 2.10 support

## 0.3.3.0 - 2022-06-21

### Added

 - Nix 2.9 support

## 0.3.2.0 - 2022-05-17

### Added

 - Nix 2.8 support

## 0.3.1.0 - 2022-04-08

### Added

 - `addTemporaryRoot`: add a store gc root for the duration of the process.

 - `isValidPath`: a simple alternative to `queryPathInfo` for simple needs.

## 0.3.0.1 - 2022-03-21

### Removed

 - Clean up inline-c generated exports

## 0.3.0.0 - 2022-03-15

### Added

 - `setVerbosity`
 - `handleException` so your program's exception handling can match that of the Nix CLI

### Removed

 - Nix 2.3 support

## 0.2.1.2 - 2022-03-07

### Fixed

 - Build with newer Nix versions 2.5, 2.6

### Added

 - Improved conditional code support with `cabal-pkg-config-version-hook`

## 0.2.1.1 - 2021-11-17

### Fixed

 - Remove unnecessary/unused/wrong compat helper in internal code

## 0.2.1.0 - 2021-09-06

### Added

 - `installDefaultSigINTHandler`

### Fixed

 - Interrupt handling

### Changed

 - Updated to work with a newer `nixUnstable`

## 0.2.0.1 - 2021-06-16

### Fixed

 - Missing file in sdist

## 0.2.0.0 - 2021-06-16

### Added

 - Speculative support for nixUnstable

 - `StorePath` (compatible for stable Nix)

 - Use and build on inline-c-cpp's TemplateHaskell-based template support

 - `nixVersion`, `storeDir`

 - Some settings getters

### Changed

 - Many functions now work with `StorePath` instead of plain paths

 - Some functions now need a `Store` because of the `StorePath` change


## 0.1.1.0 - 2021-04-21

### Added

 - Functions for retrieving the Nix daemon protocol versions
 - Functions for interacting with the interrupt handler

## 0.1.0.0 - 2021-03-05

### Added

 - First code based mostly on Hercules CI Agent and some lines from the Cachix client.
