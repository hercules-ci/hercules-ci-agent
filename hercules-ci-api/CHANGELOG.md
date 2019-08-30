# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[0.3.0.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.1.0.0...hercules-ci-api-0.3.0.0
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/compare/stable...master
