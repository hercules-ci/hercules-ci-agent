# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `hci effect run --pretend-tag` and `--as-tag` for simulating effects as if on a specific git tag.
   This sets `primaryRepo.tag` in the `herculesCI` arguments, useful for testing tag-conditional behavior such as publishing.

   Also available in `hci effect eval` and `hci effect list`.

## [0.3.9] - 2026-01-06

### Added

- `hci effect eval`: Evaluate an effect and print its derivation path.

- `hci effect list`: List all effects in a `herculesCI` configuration.

### Fixed

- `hci effect run`/`eval` now pass the `primaryRepo.owner` and `primaryRepo.name` fields correctly instead of `null`.

## [0.3.8] - 2025-07-18

### Fixed

- Interrupt signal handling is now more robust

## [0.3.7] - 2024-02-12

### Fixed

- `hci state` Commands are now more robust, retrying when HTTP requests fail

## [0.3.6] - 2023-03-07

### Added

- `hci effect run --pretend-branch` and `--pretend-ref` can now be used to replace `--as-*`.
   This command allows an effect to run locally for development, instead of on `hercules-ci-agent`.

   `--as-*` flags are still available, for those who don't want to switch right away, to remain compatible with outdated installations of `hci`.

- `--version`

### Fixed

- `hci effect run`: Attribute names with spaces, periods and/or quotes can now be specified on the command line.

## [0.3.5] - 2022-12-29

### Fixed

- Explain what to do when git's upstream HEAD is missing.

- Explicitly initialize nix on startup. This will prevent rare but obscure errors.

### Changed

- Unwrap some error messages for readability.

## [0.3.4] - 2022-12-02

### Added

- `hci state get --version INT` added, to retrieve a specific revision of a remote state file.

## [0.3.3] - 2022-11-15

### Added

- `hci effect run` can now run `onSchedule.<name>.*` effect attributes.

- Basic support for the `GitToken` secret type. You may enter a personal access token
   by hand.

### Fixed

- Fix a common error when an explicit upstream is missing but only one remote exists.
   This was sufficient data to determine the project context, and now it works.

## [0.3.2] - 2022-06-21

### Added

- Nix 2.9 support

## [0.3.1] - 2022-05-17

### Added

- `HERCULES_CI_SECRETS_JSON` can now be used to find the secrets
   in an alternate location.

- Other improvements for running in sandboxed environments, chiefly
   [`hercules-ci-effects effectVMTest`](https://docs.hercules-ci.com/hercules-ci-effects/reference/nix-functions/effectvmtest/)

## [0.3.0] - 2022-03-15

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

## [0.2.6] - 2022-03-09

### Added

- Haskell `aeson` 2.0 support

## [0.2.5] - 2022-03-07

### Added

- `hci secret echo`: write secret data on stdout. A side-effect free version of `hci secret add`.

- `hci secret echo/add --password`: another safe way of inputting a secret field

## [0.2.4] - 2021-11-17

### Added

- Improved error message when git upstream is not found

### Changed

- The flake packages and modules now link with Nix 2.4.
   Nix 2.3 support is still available via `packages.${system}.hercules-ci-cli-nix_2_3`, but will be removed in cli 0.3.x.

## [0.2.3] - 2021-10-12

### Fixed

- Remove a redundant API call that is not available using the Effect-provided token

## [0.2.2] - 2021-09-06

### Added

- `hci lock` subcommands for optional cloud locks, to be used in conjunction with state, but not enforced.

- `hci` can now run in the effects sandbox inheriting the project's context. (hercules-ci-agent >= 0.8.3)

### Fixed

- Interrupt handling

## [0.2.1] - 2021-06-22

### Added

- Compatibility with updated dependencies

## [0.2.0] - 2021-04-21

### Added

- `hci secret add`: Add `--json-env` and `--string-env`: more secure alternative for literals

### Changed

- Remove `-h` and `--help` from tab completion and help text.
- User-friendly error when `ci.nix` or similar can not be found.

## [0.1.0] - 2021-03-07

### Added

- First version of the `hci` command

[0.2.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.1.0...hercules-ci-cli-0.2.0
[0.2.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.2.0...hercules-ci-cli-0.2.1
[0.2.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.2.1...hercules-ci-cli-0.2.2
[0.2.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.2.2...hercules-ci-cli-0.2.3
[0.2.4]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.2.3...hercules-ci-cli-0.2.4
[0.2.5]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.2.4...hercules-ci-cli-0.2.5
[0.2.6]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.2.5...hercules-ci-cli-0.2.6
[0.3.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.2.6...hercules-ci-cli-0.3.0
[0.3.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.0...hercules-ci-cli-0.3.1
[0.3.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.1...hercules-ci-cli-0.3.2
[0.3.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.2...hercules-ci-cli-0.3.3
[0.3.4]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.3...hercules-ci-cli-0.3.4
[0.3.5]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.4...hercules-ci-cli-0.3.5
[0.3.6]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.5...hercules-ci-cli-0.3.6
[0.3.7]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.6...hercules-ci-cli-0.3.7
[0.3.8]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.7...hercules-ci-cli-0.3.8
[0.3.9]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-cli-0.3.8...hercules-ci-cli-0.3.9
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/compare/stable...master
