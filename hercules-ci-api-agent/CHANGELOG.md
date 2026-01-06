
# Changelog

All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]


## [0.5.1.0] - 2024-02-12

### Added

- `Mountable` configuration format
- Constant booleans in `Condition` expressions
- `SubstitutionQueryResult` evaluation event

## [0.5.0.1] - 2023-06-28

- Maintenance release

## [0.5.0.0] - 2023-03-06

### Added

- GitLab endpoints
- `onSchedule` information
- Various small additions

### Changed

- `SourceHostingSite` was renamed to `Forge`


## [0.4.6.1] - 2022-12-29

Maintenance and tooling update.

## [0.4.6.0] - 2022-11-15

### Added

- `onSchedule` event handler types and operations
- `GitToken` secrets type, secrets types
- `ImmutableGitInput` repository metadata attributes

## [0.4.5.0] - 2022-05-17

### Added

- `AttributeIFDEvent`
- `ResultTypeSetExpected`

## [0.4.4.0] - 2022-03-30

### Added

- A field for traces in evaluation errors (`--show-trace`)

## [0.4.3.0] - 2022-03-18

### Added

- `isFlake` and `isFlakeJob` fields for to support an optimized checkout flow
   supporting access of flake metadata fields like `sourceInfo`.

## [0.4.2.0] - 2022-03-15

### Added

- "Selectors" to distinguish `OnPush`, `IsConfig` event, etc
- Evaluation task: `ciSystems` parameter, `OnPush` selector, `extraGitCredentials`
- Secrets: `condition` parameter

### Changed

- aeson: 1 -> 2

## [0.4.1.2] - 2022-03-09

### Added

- Haskell `aeson` 2.0 support

## [0.4.1.1] - 2022-03-07

### Fixed

- Flaky test

## [0.4.1.0] - 2021-09-06

### Added

- `EffectTask` fields to support running `hci` in effects

## [0.4.0.0] - 2021-06-22

### Added

- `ResultTypeBuildLogLine`

### Changed

- `OutputInfo.path` is optional

## [0.3.1.0] - 2021-04-21

### Added

- Attach user-defined labels to agent
- Attach Nix protocol versions to logs and agent

## [0.3.0.0] - 2021-03-07

### Added

- Input source metadata field for src.ref, src.rev
- Effects tasks
- A non-API type for the secrets in secrets.json
- State file API
- Attribute types, including MustFail, DependenciesOnly, Effect

### Removed

- `ToSchema` instances

## [0.2.2.0] - 2020-07-18

### Added

- Evaluation log

## [0.2.1.0] - 2020-06-17

### Added

- Add a `NixCache` format for generic binary cache support using Nix's generic "URI"-based store API.

## [0.2.0.0] - 2020-05-05

Start of changelog


[0.2.1.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.2.0.0...hercules-ci-api-agent-0.2.1.0
[0.2.2.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.2.1.0...hercules-ci-api-agent-0.2.2.0
[0.3.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.2.2.0...hercules-ci-api-agent-0.3.0.0
[0.3.1.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.3.0.0...hercules-ci-api-agent-0.3.1.0
[0.4.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.3.1.0...hercules-ci-api-agent-0.4.0.0
[0.4.1.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.0.0...hercules-ci-api-agent-0.4.1.0
[0.4.1.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.1.0...hercules-ci-api-agent-0.4.1.1
[0.4.1.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.1.1...hercules-ci-api-agent-0.4.1.2
[0.4.2.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.1.2...hercules-ci-api-agent-0.4.2.0
[0.4.3.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.2.0...hercules-ci-api-agent-0.4.3.0
[0.4.4.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.3.0...hercules-ci-api-agent-0.4.4.0
[0.4.5.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.4.0...hercules-ci-api-agent-0.4.5.0
[0.4.6.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.5.0...hercules-ci-api-agent-0.4.6.0
[0.4.6.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.6.0...hercules-ci-api-agent-0.4.6.1
[0.5.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.4.6.1...hercules-ci-api-agent-0.5.0.0
[0.5.0.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.5.0.0...hercules-ci-api-agent-0.5.0.1
[0.5.1.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-api-agent-0.5.0.1...hercules-ci-api-agent-0.5.1.0
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/compare/stable...master
