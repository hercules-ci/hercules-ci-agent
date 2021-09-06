# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

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
