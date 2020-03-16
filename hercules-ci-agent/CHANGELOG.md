# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.6.6] - 2020-03-16

### Fixed

 - NixOS, nix-darwin modules: check the `nix-daemon` source and add option to patch an in-memory cache expiry issue
   causing errors in build clusters (of more than 1 machine). The module asks for confirmation.

 - **Manual action:** if you are not using the provided module and you run the agent on
   more than one machine, and you use `nix-daemon`, to fix above issue, you need to:
    - either upgrade your system's Nix to a recent `master` or version 2.4.0 when released,
    - or apply this patch to your system's Nix installation: https://github.com/NixOS/nix/pull/3405

 - Cachix: 0.3.5 -> [0.3.7](https://github.com/cachix/cachix/blob/master/cachix/CHANGELOG.md#037---2020-03-12) to prevent uploading bad NARs in rare cases.

## [0.6.5] - 2020-03-07

### Fixed

 - Work around a systemd behavior where it didn't restart the unit

### Added

 - `--test-configuration` flag to validate the configuration without actually running the agent.

## [0.6.4] - 2020-03-06

### Fixed

 - Fix a bug blocking evaluation when a store path is removed from cache or cache configuration is changed.

### Added

 - Cached builds to speed up `aarch64-linux` agent deployments.

### [0.6.3] - 2020-02-19

### Fixed

 - Fix a concurrency problem causing not all evaluation events to be written to server when evaluation fails.

 - Fix evaluation errors triggered by build outputs go missing from cache, by requesting a forced rebuild.

 - Fix blocked shutdown on NixOS, fix agent status in dashboard, by stopping agent before network shutdown #195.

 - Fix upload of large outputs by using the correct http client manager for Cachix, removing a timeout.

### Changed

 - Agent will now try to verify that the nix-daemon has narinfo-cache-negative-ttl = 0. This is required for correct operation.

### [0.6.2] - 2020-01-30

### Fixed

 - Update cachix to support the API change for the new CDN. This update is
   required for uploading sources and compressed outputs over 100MB in size.
   Please update.

### [0.6.1] - 2019-11-06

### Fixed

 - Fix token leak to system log when reporting an HTTP exception. This was introduced by a library upgrade.
   This was discovered after tagging 0.6.0 but before the release was
   announced and before moving of the `stable` branch.
   Only users of the `hercules-ci-agent` `master` branch and the unannounced
   tag were exposed to this leak.
   We recommend to follow the `stable` branch.

 - Temporarily revert a Nix GC configuration change that might cause problems
   until agent gc root behavior is improved.

### [0.6.0] - 2019-11-04

### Changed

 - Switch to Nix 2.3 and NixOS 19.09. *You should update your deployment to reflect the NixOS upgrade*, unless you're using terraform or nix-darwin, where it's automatic.
 - Increased parallellism during push to cachix
 - Switch to NixOS 19.09
 - Enable min-free/max-free Nix GC

### Fixed

 - Transient errors during source code fetching are now retried
 - Fixed a bug related to narinfo caching in the context of IFD
 - Fixed an exception when the root of ci.nix is a list, although lists are unsupported

## [0.5.0] - 2019-10-01

### Added

- Now deployable with [terraform-hercules-ci](https://github.com/hercules-ci/terraform-hercules-ci)

### Changed

- The `binary-caches.json` file can now be deployed like any other confidential file. Its contents are not required at module evaluation time any more.

- The `services.hercules-ci-agent.binaryCachesFile` option has been removed.

  **NixOps users**: rename to `deployment.keys."binary-caches.json".file`

  **Others**: remove your `binaryCachesFile` value. Make sure `binary-caches.json` is deployed.

- The `binary-caches.json` file is now required. The empty object `{}` is a
  valid file, but we highly recommend to configure a cache.


### Fixed

 - The agent will now actually auto-restart when the secrets files change.


## [0.4.0] - 2019-08-30

### Added

- Support for import-from-derivation. See https://blog.hercules-ci.com/2019/08/30/native-support-for-import-for-derivation/ for details.

### Changed


- Report build failures and technical errors (misconfigurations, etc) separately

- Remove HerculesScribe

- Worker now uses structured logging (including worker pid, etc)

### Fixed

- Disable parallel GHC GC to improve runtime performance

- Bump Cachix to fix a few bugs (errors with too many derivations, performance fixes, etc.)

 - Modern BoehmGC initial settings for Nix memory limits 

## [0.3.2] - 2019-08-11

### Fixed

- Deploying the agent from different system (darwin to linux) resulted into
  using the wrong executable


## [0.3.1] - 2019-08-07

### Changed

- Emit a log when evaluator starts to push to cachix

- Increase attribute limit to 50k

- Pin nixpkgs commit and speed up compilation via https://hercules-ci.cachix.org


### Fixed

- Possible exception during evaluation was not propagated,
  resulting into lack of retries

- #8: Refresh agent session on cluster join token change

- Fix segfault on involved IFD project (remove a finalizer)

- Cachix: fix a crash with a lot of attributes (when determining closure graph)

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

[0.6.6]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.4...hercules-ci-agent-0.6.6
[0.6.5]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.3...hercules-ci-agent-0.6.5
[0.6.4]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.3...hercules-ci-agent-0.6.4
[0.6.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.2...hercules-ci-agent-0.6.3
[0.6.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.1...hercules-ci-agent-0.6.2
[0.6.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.0...hercules-ci-agent-0.6.1
[0.6.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.5.0...hercules-ci-agent-0.6.0
[0.5.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.4.0...hercules-ci-agent-0.5.0
[0.4.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.3.2...hercules-ci-agent-0.4.0
[0.3.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.3.1...hercules-ci-agent-0.3.2
[0.3.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.3.0...hercules-ci-agent-0.3.1
[0.3.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.2...hercules-ci-agent-0.3.0
[0.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.1.1...hercules-ci-agent-0.2
[0.1.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.1.0.0...hercules-ci-agent-0.1.1
[Unreleased]: https://github.com/hercules-ci/hercules-ci-agent/compare/stable...master
[nix-gitignore]: https://github.com/siers/nix-gitignore
[gitignore]: https://github.com/hercules-ci/gitignore
