# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## Unreleased

 - `hci effect run` can now run `onSchedule.<name>.*` effect attributes.

## 0.3.2 - 2022-06-21

### Added

 - Nix 2.9 support

## 0.3.1 - 2022-05-17

### Added

 - `HERCULES_CI_SECRETS_JSON` can now be used to find the secrets
   in an alternate location.

 - Other improvements for running in sandboxed environments, chiefly
   [`hercules-ci-effects effectVMTest`](https://docs.hercules-ci.com/hercules-ci-effects/reference/nix-functions/effectvmtest/)

## 0.3.0 - 2022-03-15

### Added

 - `hci secret echo` to assemble a secret and print it on stdout.
   Not unlike `hci secret add` but for people who don't have local
   secrets as part of their setup.

 - `hci secret add/echo --password` to ask a password on the terminal.

 - `hci secret add/echo` add a default `condition` to the secret.

 - Parity with hercules-ci-agent 0.9.0: flake support, `onPush` jobs
   in `hci effect run`.

### Fixed

 - Nix warnings don't pollute the shell completions anymore

## 0.2.6 - 2022-03-09

### Added

 - Haskell `aeson` 2.0 support

## 0.2.5 - 2022-03-07

### Added

 - `hci secret echo`: write secret data on stdout. A side-effect free version of `hci secret add`.

 - `hci secret echo/add --password`: another safe way of inputting a secret field

## 0.2.4 - 2021-11-17

### Added

 - Improved error message when git upstream is not found

### Changed

 - The flake packages and modules now link with Nix 2.4.
   Nix 2.3 support is still available via `packages.${system}.hercules-ci-cli-nix_2_3`, but will be removed in cli 0.3.x.

## 0.2.3 - 2021-10-12

### Fixed

 - Remove a redundant API call that is not available using the Effect-provided token

## 0.2.2 - 2021-09-06

### Added

 - `hci lock` subcommands for optional cloud locks, to be used in conjunction with state, but not enforced.

 - `hci` can now run in the effects sandbox inheriting the project's context. (hercules-ci-agent >= 0.8.3)

### Fixed

 - Interrupt handling

## 0.2.1 - 2021-06-22

### Added

 - Compatibility with updated dependencies

## 0.2.0 - 2021-04-21

### Added

 - `hci secret add`: Add `--json-env` and `--string-env`: more secure alternative for literals

### Changes

 - Remove `-h` and `--help` from tab completion and help text.
 - User-friendly error when `ci.nix` or similar can not be found.

## 0.1.0

### Added

 - First version of the `hci` command
