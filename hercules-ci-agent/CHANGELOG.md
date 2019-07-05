# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0] - 2019-07-05


### Changed

- Configuration of the agent is now done via `--config agent.toml`
  so all command line arguments were removed.

  See https://docs.hercules-ci.com/#agent-configuration-file

- NixOS-based deployments now require `enable`.

      services.hercules-ci-agent.enable = true;

- All files are placed/expected in new locations that by default derive
  from the `baseDirectory` option in the `agent.toml` file.

  You may remove `~/.hercules-ci-agent` and `~/.local/share/hercules-ci-agent` after upgrading.

### Fixed

- Added retries to status reporting to fix potential
  inconsistencies on the service

### Added

- Added Cachix support, for multi-agent and multi-platform support

- Report derivation outputs with their size and hashes

- Added Darwin support via nix-darwin

- Support `requiredFeatures` attribute on derivations

- Hello and hearthbeat protocol, which will allow the
  service to be aware of how the agent is configured and
  when it's online.

## [0.2] - 2019-05-14

- use [gitignore] instead of [nix-gitignore]
- fix build on Darwin
- limit internal concurrency to max eight OS threads for beefier machines
- show version on `--help`
- build against NixOS 19.03 as default
- propagate agent information to agent view: Nix version, substituters,
  platform and Nix features

## [0.1.1] - 2019-04-16

### Added

- Support ci.nix or nix/ci.nix along with default.nix

## [0.1.0.0] - 2019-03-28

- Initial release

[0.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.2...hercules-ci-agent-0.3
[0.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.1.1...hercules-ci-agent-0.2
[0.1.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.1.0.0...hercules-ci-agent-0.1.1
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/compare/stable...master
[nix-gitignore]: https://github.com/siers/nix-gitignore
[gitignore]: https://github.com/hercules-ci/gitignore
