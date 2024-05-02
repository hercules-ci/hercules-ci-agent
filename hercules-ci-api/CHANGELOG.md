# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.8.3.0 - 2024-05-03

### Fixed

- Deduplicate OpenAPI3 security / jwt items
- Replace bad generated OpenAPI3 identifier

### Added
- `O3.ToParamSchema JobType`
- `instance HasField "get"` for `PagedJobs`
- `getJobs`: Add `rev`, `handler`, `name` query params
- `instance ToParamSchema JobType`
- `Hercules.API.ShowRead` wrapper
deea4 doc: API / Find jobs: refer to per project endpoint
- `NotPlanned` derivation status
- Document auth instructions in API description
- Add `references` to `BuiltOutput`

## 0.8.2.0 - 2024-02-12

### Added

 - Experimental OpenAPI 3 generation

## 0.8.1.0 - 2023-06-28

### Added

 - `getJob` (by id)
 - `projectJobEvaluationDiff` for diffing a pair of evaluations
 - `Job.{repo,owner,forge}Name`

## 0.8.0.0 - 2023-03-06

### Changed

 - `SourceHostingSite` was renamed to `Forge`

### Added

 - GitLab endpoints
 - `onSchedule` information
 - Various small additions


## 0.7.2.1 - 2022-12-29

Maintenance and tooling update.

## [0.7.2.0] - 2022-03-15

### Added

 - Add getJobSource to resolve extraInputs in hci
 - `PagedResponse`
 - `SimpleProject`
 - `SimpleRepo`
 - `ImmutableGitInput`
 - `JobType`

## [0.7.1.0] - 2021-09-06

### Added

 - Notification settings
 - Email info
 - State locks: opt-in locks to be used in conjunction with state files. Use of locks is not enforced.

### Changed

 - `DerivationOutput.outputPath` is now nullable when retrieving build info

## [0.7.0.0] - 2021-06-22

### Added

 - Account: manageInstallationURL, installationIsSelection
 - AccountInstallationStatus
 - signOut
 - Agent labels
 - By name variations of account and state endpoints

## [0.6.0.1] - 2020-04-21

### Fixed

 - A warning

## [0.6.0.0] - 2020-03-07

### Added

 - Build logs
 - Evaluation log
 - Attribute types
 - Effects
 - State files
 - CLI authorization flow
 - Endpoint for resolving git urls to projects

### Changed

 - Adaptations to support servant streaming

## [0.5.0.0] - 2020-01-30

### Changed

- `hercules-ci-api-core` and `hercules-ci-api-agent` packages have been extracted.

### Added

- Account settings
- Project enable/disable
- Some authorization-related fields
- Billing
- Derivation info and events
- Job derivation statistics and improved status reporting
- Derivation and Job restarts
- Job cancellation

## [0.4.0.0] - 2019-08-30

### Fixed

- Swagger schema was wrong for a Result
- Swagger schema is now written using UTF-8 encoding

### Changed

- /jobs endpoint changed to be hierarchical

### Added

- Submit number of concurrent tasks to the backend for better scheduling of evaluations (to avoid IFD deadlocks)

- New endpoint to return evaluation build dependencies for IFD


## [0.3.0.0] - 2019-07-05

### Added

- The CachixCache JSON format for configuring binary caches.

- Endpoints for lifecycle management: hello, hearbeat, goodbye.

- requiredFeatures support

## 0.1.0.0

Initial release

[0.7.1.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.7.0.0...hercules-ci-api-0.7.1.0
[0.7.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.0.1...hercules-ci-api-0.7.0.0
[0.6.0.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.0.0...hercules-ci-api-0.6.0.1
[0.6.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.5.0.0...hercules-ci-api-0.6.0.0
[0.5.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.4.0.0...hercules-ci-api-0.5.0.0
[0.4.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.3.0.0...hercules-ci-api-0.4.0.0
[0.3.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.1.0.0...hercules-ci-api-0.3.0.0
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/compare/stable...master
