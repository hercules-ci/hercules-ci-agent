# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)

## [0.1.6.0] - 2024-02-12

### Added

- OpenAPI 3 support
- `newtype Sensitive`, a generic wrapper with a constant `Show` instance

## [0.1.5.1] - 2023-06-28

### Added

 - Make some types potentially more general by removing redundant constraints

## [0.1.5.0] - 2022-11-15

### Added

 - `DayOfWeek`

## [0.1.4.0] - 2022-03-15

### Updated

 - aeson: 1 -> 2
 - GHC 9.0 support

## [0.1.3.0] - 2021-06-22

### Added

- Utilities for servant's `Generic` feature
  - `Substitute`
  - `useApiE`, `enterApi`, `enterApiE`

## [0.1.2.0] - 2020-03-07

- Use `uuid` type in swagger
- Add `NFData` instances

## [0.1.1.0] - 2020-05-05

- Allow `Id` in JSON object keys

## [0.1.0.0] - 2020-01-30

- Initial release, split out of hercules-ci-api

[0.1.5.0]: https://github.com/hercules-ci/hercules-ci-agent/releases/tag/hercules-ci-api-core-0.1.5.0
[0.1.4.0]: https://github.com/hercules-ci/hercules-ci-agent/releases/tag/hercules-ci-api-core-0.1.4.0
[0.1.3.0]: https://github.com/hercules-ci/hercules-ci-agent/releases/tag/hercules-ci-api-core-0.1.3.0
[0.1.2.0]: https://github.com/hercules-ci/hercules-ci-agent/releases/tag/hercules-ci-api-core-0.1.2.0
[0.1.1.0]: https://github.com/hercules-ci/hercules-ci-agent/releases/tag/hercules-ci-api-core-0.1.1.0
[0.1.0.0]: https://github.com/hercules-ci/hercules-ci-agent/releases/tag/hercules-ci-api-core-0.1.0.0
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/compare/stable...master
