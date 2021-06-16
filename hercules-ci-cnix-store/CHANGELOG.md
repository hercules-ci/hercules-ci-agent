
# Changelog

All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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
