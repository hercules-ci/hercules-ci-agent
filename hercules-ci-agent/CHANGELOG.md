# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.10.6] - 2024-05-05

### Added

- Nix 2.28 support

### Fixed

- Always fetch dependency closure when max-jobs == 0

## [0.10.5] - 2024-11-15

### Added

 - Nix 2.24 support

## [0.10.4] - 2024-06-12

### Fixed

 - Embed data file to make packaging easier.

## [0.10.3] - 2024-05-19

### Fixed

 - Updates in the Nix bindings, improving stability.

## [0.10.2] - 2024-05-03

### Added

 - Add `nixSettings` configuration item, to more easily configure Nix settings such as `substituters`, overriding the system Nix settings.
 - Cachix 1.7.2 support

### Fixed

 - Remove most uses of `c_str()`, of which at least one exhibited undefined behavior.
 - `nix-darwin` module now reads `system` correctly from the configuration.

## [0.10.1] - 2024-02-12

### Changed

 - More work is performed concurrently during evaluation, including binary cache lookups and (more) build dispatch. This results in a speedup.

 - Dependencies of build dependencies are not scheduled eagerly anymore.
   This reduces the scope of all jobs that are evaluated by agents since this release, resulting in a speedup.
   This resolves a noticable slowdown when first evaluating significant Nixpkgs updates when its `staging` branch is merged.

   Strictly speaking, a job success no longer guarantees that absolutely everything (all the way up to the bootstrap binaries) is realisable on your agents.
   This property is generally not your responsibility, and enforcing it had the effect of excluding less reproducible platforms such as darwin.
   Instead, a weaker property is provided: *your* derivations are realisable, as well as the immediate build dependencies. "Your derivations" is defined as those whose outputs are not already cached.

   CI setups based on the Nix command line interface (almost all CIs) also behave this way.

 - The recommended configuration format is now **JSON**, preferably generated using a configuration manager such as NixOS or nix-darwin.
   TOML is still supported, but does not support `null` in labels, and due to library limitations, it requires that intermediate tables be specified. See [the config file documentation](https://docs.hercules-ci.com/hercules-ci-agent/agent-config).

 - `services.hercules-ci-agent` is now an alias for `services.hercules-ci-agents.""`, which still provides the same behavior as the old module.

 - Hardening flags have been applied to the NixOS module.

 - The effect sandbox now use the `crun` container runtime instead of `runc`.

 - Attribute sets containing a `_type` attribute are not scanned for derivations in `herculesCI.<...>.outputs`. This prevents accidental scanning of large or failing attribute trees, such as NixOS configurations. `nixosConfigurations` in Flakes are still built as usual, as they are not (verbatim) in the `herculesCI.<...>.outputs` attributes.

### Added

 - Effect mounts. Specify [`effectMountables`](https://docs.hercules-ci.com/hercules-ci-agent/agent-config.html#effectMountables) in the agent configuration, deploy, and [mount](https://docs.hercules-ci.com/hercules-ci-agent/effects/declaration.html#__hci_effect_mounts) them into an effect. This can be used for instance to expose the host's `/etc/hosts`, or hardware devices such as GPUs. Access is [controlled](https://docs.hercules-ci.com/hercules-ci-agent/agent-config.html#effectMountables-condition) by the agent configuration.

 - New configuration option `remotePlatformsWithSameFeatures`, allowing a remote build to be used before more elaborate remote builder support is implemented.
   The recommended method for running a cluster is still to install `hercules-ci-agent` on each machine, as that is more efficient and accurate.

 - Agent [labels](https://docs.hercules-ci.com/hercules-ci-agent/agent-config.html#labels) can now be `null`, when using the JSON configuration format.

### Fixed

 - Low level crash details are now reported in the log as expected.

 - An interaction between the Nix GC and threads has been fixed, solving such a crash.

## [0.9.12] - 2023-06-28

### Added

 - Nix 2.16 support

 - Nix 2.15 support

### Fixed

 - Do not `chdir` the build worker. This functionality of the `process` package appears unreliable, but is not needed.

## [0.9.11] - 2023-03-06

### BREAKING

 -  The `nix-darwin` module uses new user id and group id numbers, to match the upstream `nix-darwin` module.
    The main benefit of the upstream change is that the agent user will not appear as a normal (human) user on the system.

    To migrate, run:

    ```shell
    # if you have deployed an older version of the nix-darwin PR before,
    # the following delete commands will prevents this error:
    #     creating group _hercules-ci-agent...
    #     <main> attribute status: eDSRecordAlreadyExists
    #     <dscl_cmd> DS Error: -14135 (eDSRecordAlreadyExists)
    sudo dscl . -delete '/Groups/_hercules-ci-agent'
    sudo dscl . -delete '/Users/_hercules-ci-agent'

    # update the flake inputs / expressions / channels
    # and deploy the new version, e.g:
    sudo darwin-rebuild switch

    sudo chown -R _hercules-ci-agent:_hercules-ci-agent /var/lib/hercules-ci-agent
    ```

### Fixed

 - Non-builder build errors such as output cycles are now reported
   in the build log.

 - `ciSystems` is now taken into account by the default `onPush.default`
   job when the `herculesCI` attribute of a flake is a function.

 - `darwinConfigurations` is now filtered in accordance with `ciSystems`.


### Added

 - Nix verbosity can now be specified in the config file under the
   new attribute `nixVerbosity`.

 - Cachix 1.3 support

 - Nix 2.14 support

 - Nix 2.13 support


## [0.9.10] - 2022-12-29

### Fixed

 - Detect stack overflows correctly in Nix evaluation

 - Retry errors from Nix-native (non-cachix) caches

### Added

 - Cachix 1.1 compatibility

### Changed

 - Unwrap some error messages for readability

## [0.9.9] - 2022-12-02

### Fixed

 - The flake `templates` check now allows filtered sources.

 - Tweaks to diagnostic messages

## [0.9.8] - 2022-11-15

### Added

 - `herculesCI.onSchedule` jobs, which a created at set times. These can be used
   to automate work that doesn't start with a code change, such as automatic
   updates or impure periodic deployments.

 - Secret types and the `GitToken` type, to be provided by the Hercules CI GitHub
   App. As of writing this requires a permission approval in the GitHub UI. This
   has to be initiated on the Hercules CI side. More on that very soon.

 - More repository metadata in the `herculesCI` attribute; see [evaluation docs](https://docs.hercules-ci.com/hercules-ci-agent/evaluation/#param-herculesCI-primaryRepo).

### Fixed

 - Work around excessive stack use by libstdc++ regex [issue](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=86164)
   The new limit of 256 MiB stack allows larger string inputs to be used.
   This bug was triggered by purs-nix and possibly other Nix expression libraries that parse substantial files.

 - Lift the agent-side restriction on the number of derivations in a job.

## [0.9.7] - 2022-07-21

### Added

 - Nix 2.10 support

### Removed

 - `-nix_2_7` variants. Nixpkgs has stopped offering older versions of Nix. If
   you need an older version, you could inject an older Nixpkgs into this flake
   using `hercules-ci-agent.inputs.nixpkgs.follows = ...`.

## [0.9.6] - 2022-06-21

### Added

 - Nix 2.9 support

### Fixed

 - NixOS module: keep main process running when worker triggers system OOM killer.
 - Missing file in hackage sdist

## [0.9.5] - 2022-05-17

### Added

 - Concurrent IFD, reducing evaluation wall clock time
 - Nix 2.8.0 support
 - Improved log contexts

### Fixed

 - Workaround for cachix#406 (add `login` to `netrc`)
 - A crash in `inline-c-cpp` exception handling (`inline-c-cpp` update)
 - Towards the error "Could not push logs within 10 minutes after completion"
    - Add a timeout to prevent hang in case of a stuck handshake
    - Enforce log limit on client side as well in case of excessive log spam and an upload bottleneck

### Removed

 - `hercules-ci-agent-nix_2_5` variant: upgrade to plain `hercules-ci-agent` (2.8.0) or `_nix_2_7`.

## 0.9.4 - 2022-05-17

Bad release. See 0.9.5 for changes.

## [0.9.3] - 2022-04-08

### Added

 - The evaluator now caches build statuses and ignores redundant rebuild requests, giving a significant performance boost to IFD-heavy jobs.

### Fixed

 - The branch name in flake-based jobs is now handled correctly when special characters are present.

## [0.9.2] - 2022-03-30

### Added

 - Separate traces in the dashboard (as in `--show-trace`)

### Fixed

 - Effects: `error: cannot open connection to remote store 'daemon': error: reading from file: Connection reset by peer`

## [0.9.1] - 2022-03-18

### Added

 - The built-in flake support now has the `sourceInfo` attributes.

### Changed

 - Flakes are checked out by Nix rather than custom local checkout.

## [0.9.0] - 2022-03-15

This release comes with an [Upgrade Guide! ✨](https://docs.hercules-ci.com/hercules-ci/guides/upgrade-to-agent-0.9/)

### Added

 - Flakes support!

   Instead of needing a `ci.nix`, the agent will pick up `flake.nix` and look
   for the [`herculesCI`](https://docs.hercules-ci.com/hercules-ci-agent/evaluation/#_herculesci_value) attribute in the flake.

   Only the `outputs.effects` sub-attributes may define effects, making attacks on secrets harder to conceal.

 - Multiple jobs per commit

 - Jobs that run with the latest successful dependency build

 - Conditions on secrets, disallowing access to secrets except when the conditions are met. This enforces the four eyes principle when branch protection is set up to match the secrets' conditions.
   A missing `condition` field does not give a great error message for security reasons, so follow the [upgrade guide](https://docs.hercules-ci.com/hercules-ci/guides/upgrade-to-agent-0.9/).

 - Hardening against rogue contributors. Trivial attacks trying to read system paths or secrets are no longer possible. Similar to typical CIs, secrets _can_ be stolen under specific circumstances: either a misconfiguration of branch protection or by approval of a second maintainer. Note that issue was already largely addressed by only processing contributions from GitHub users with write access to the repository, which also still applies.

 - Built-in support for fetching private repositories and tarballs.

### Changed

 - File lookup order has changed, to support flakes. `ci.nix` or `nix/ci.nix` still take top priority, followed by `flake.nix`, followed by `default.nix`.

 - Installed private repositories can now be read by a collaborator. If you need to enforce confidentiality across repositories, contact us and use a personal access token with appropriate permissions in the meanwhile.

### Fixed

 - When the root of a `ci.nix` is a list, an error message is returned.

### Removed

 - Nix 2.3 support

## [0.8.7] - 2022-03-09

### Added

 - Nix 2.7 support
 - Haskell `aeson` 2.0 support

## [0.8.6] - 2022-03-07

### Fixed

 - Build with newer Nix versions 2.5, 2.6

### Added

 - Improved conditional code support with `cabal-pkg-config-version-hook`

## [0.8.5]

### Added

 - The flake now has `.nixosModules.multi-agent-service` allowing multiple agents
   to run on the same system.
   An instance with default settings can be enabled with `services.hercules-ci-agents."some-name" = {}`.
   User name and file paths are like the regular module, except replacing `hercules-ci-agent` by `hci-${name}` if the chosen `name` is not `""`.
   `services.hercules-ci-agents."" = {}` is equivalent to `services.hercules-ci-agent.enable = true`.

### Fixed

 - Fix mounting `/etc/resolv.conf`, work around runc#1523. Fixed by @Mic92 in #357

 - An issue where a Nix evaluator crash could lead to builds being triggered in
   the backend for which the derivation hadn't been pushed to the cache yet,
   causing needless build failures. #314

 - A build error caused by a moved symbol in `cachix >= 0.7`. #363

 - A test that relied on `aeson` field order, which isn't stable. #352

## [0.8.4]

### Added

 - The path to `secrets.json` is now configurable in the module or config file,
   using the `secretsJsonPath` setting, analogous to `clusterJoinTokenPath`.

 - `aarch64-darwin` is now officially supported.

 - All module settings options will be visible in the NixOS documentation.
   Some less-used settings were hidden, specifically the file path options
   that default to `staticSecretsDirectory + "/cluster-join-token.key"`, etc.

### Changed

 - The flake packages and modules now link with Nix 2.4.
   Nix 2.3 support is still available via `packages.${system}.hercules-ci-agent-nix_2_3`, but will be removed in agent 0.9.x.

 - No longer patch Boehm GC, staying closer to regular Nix and the Nixpkgs build of the `hercules-ci-agent`.

### Fixed

 - Various fixes related to the upgrade to Nix 2.4.

## [0.8.3]

### Added

 - `hci` can now run in the effects sandbox

### Fixed

 - Interrupt handling has been improved

## [0.8.2]

### Added

 - Preparations for the next Nix version

### Fixed

 - #304, `message:epollControl: invalid argument (Bad file descriptor)` in effect task

## [0.8.1]

### Added

 - Attach user-defined labels to the agent, for retrieval via the API.

### Fixed

 - Fix an issue where long-runnin nix methods weren't interrupted
 - Fix error `mkdir /run/runc: permission denied`

## [0.8.0]

### Added

 - Hercules CI Effects, a new feature for running programs that interact with
   the real world, with useful features for continous deployment.

    - Effects only run after the build completes successfully

    - Effects are defined like a derivation, not unlike a Nix shell

    - Independent processes can run concurrently as distinct effects

    - No two commits in the same repo run effects at the same time; no need to
      worry about concurrency in deployment scripts

    - Effects each run in their own sandbox with access to network, Nix store,
      remote state file API and secrets

    - Secrets are configured locally on your agents, so you don't have to trust
      a third party with your cloud credentials

 - Hercules CI Agent is now a flake. The highlights are
    - `nixosModules` overriding the NixOS-distributed module to the in-repo version
      - `agent-profile` for agent machines, or
      - `agent-service` for just the service definition
    - `packages`
      - `hercules-ci-cli` the user command line interface
      - `hercules-ci-agent` for custom installation methods, etc

 - The `hci` command (flake: `defaultApp`)
    - `hci login` to authenticate yourself
    - `hci state` to work with Effects state files
    - `hci effect` to run effects locally

 - Commit metadata as a `ci.nix` argument. Make your `ci.nix` a function:

   ```nix
   { src ? { ref = null; rev = null; }}:
   # rest of your ci.nix
   ```

   `src.ref` will have e.g. `refs/heads/master` and `rev` will have the
   git commit SHA.

 - Shell derivations will only be built for their dependencies. Add a
   `mkShell`-based expression like you would add a derivation.

   This behavior can be requested explicitly for shells and non-shell
   derivations alike by appending ` // { buildDependenciesOnly = true; }` to
   the attribute definition.

 - Attributes can now be marked to require or ignore a build failure in the
   derivation it references directly.
   (see [support#34](https://github.com/hercules-ci/support/issues/34))

 - `concurrentTasks` now has a default, `"auto"` for ease of setup and to help
   avoid underutilization.

### Fixed

 - The parent directory name will match the repo name [support#40](https://github.com/hercules-ci/support/issues/40)

 - Previously, lines from Nix's configured netrc file were ignored. Now they are appended to Hercules CI's netrc lines.

### Changed

 - Cachix caches without `signingKeys` will be pushed to, as part of the recently
   introduced write token feature (Cachix-managed signing keys)


## [0.7.5] - 2020-11-27

### Fixed

 - GHC 8.10.2 compatibility for NixOS unstable / NixOS 21.03

 - Build with cachix 0.5.1. Write token support is not included due to a break configuration semantics. It will be available in >= 0.8.

### Changed

 - The in-repo expressions have upgraded their dependency to NixOS 20.09

 - Agent will now use `HERCULES_CI_API_BASE_URL` over `HERCULES_API_BASE_URL` if set.

 - Temporarily switch to cabal tooling due to breakage. `master` continues to be stack-based.

## [0.7.4] - 2020-08-25

### Fixed

 - Paths that are missing from the binary cache will be rebuilt. This affected
   agent 0.7 - 0.7.3 users with trusted user optimizations turned on, which is
   the default when using the NixOS or nix-darwin modules.

 - Prevent states where no progress can be made. One caused by a potential buildup
   of batched messages that may not fit within the timeout interval; the other
   a receive operation without a timeout during the initial socket handshake.

 - The log socket will remain open instead of reconnecting unnecessarily.

 - Add a safety measure to prevent unintended increases in workload in case
   Nix sees an opportunity for concurrency that was not intended by Hercules CI.

### Changed

 - The NixOS module in the hercules-ci-agent repo now disables the upcoming
   module that is packaged upstream with NixOS.

   The upstream module will configure fewer things for you, to be in line with
   normal NixOS expectations. Notably, it does not configure automatic garbage
   collection and it does not preconfigure NixOps keys deployment.

   The configuration interface in the hercules-ci-agent repo will remain unchanged for `0.7` but `0.8` will
   match the upstream interface.

## [0.7.3] - 2020-07-18

### Added

 - Evaluation log

 - Configurable log level via config file or `extraOptions`

### Changed

 - Default log level is `InfoS` rather than `DebugS`

## [0.7.2] - 2020-06-18

### Changed

 - cachix: 0.3.7 -> 0.3.8. Improves reliability through better retries and improves error reporting

## [0.7.1] - 2020-06-17

### Added

 - Push to any nix store, including S3, using the `NixCache` kind in `binary-caches.json`

### Changed

 - Switch to Nixpkgs 20.03

 - Environment variables are now passed on to evaluation and build by default. This allows configuration to be passed to Nix without intervention from hercules-ci-agent.

 - `trusted-user` is not a requirement and is configured automatically when using the NixOS module or nix-darwin module.

### Fixed

 - Agent will now reset its connection with hercules-ci.com when pings are not acknowledged in time.

 - Prevent running out of file descriptors by increasing the soft limit if possible.

## [0.7.0] - 2020-05-05

### Known issues

 - Agent process user must be in `trusted-users`. This is the case with the NixOS and nix-darwin module. Doing so is recommended for ease of use and performance but should not be a requirement.

### Added

 - Jobs can be cancelled

 - Build logs are streamed in realtime

 - The build log now has timestamps and color

 - Distributed builds performance has improved by fetching fewer derivations

### Fixed

 - Extra nix options were not passed to the build process

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

## 0.1.0.0 - 2019-03-28

- Initial release

[0.10.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.10.2...hercules-ci-agent-0.10.3
[0.10.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.10.1...hercules-ci-agent-0.10.2
[0.10.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.12...hercules-ci-agent-0.10.1
[0.9.12]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.11...hercules-ci-agent-0.9.12
[0.9.11]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.10...hercules-ci-agent-0.9.11
[0.9.10]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.9...hercules-ci-agent-0.9.10
[0.9.9]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.8...hercules-ci-agent-0.9.9
[0.9.8]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.7...hercules-ci-agent-0.9.8
[0.9.7]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.6...hercules-ci-agent-0.9.7
[0.9.6]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.5...hercules-ci-agent-0.9.6
[0.9.5]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.3...hercules-ci-agent-0.9.5
[0.9.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.2...hercules-ci-agent-0.9.3
[0.9.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.1...hercules-ci-agent-0.9.2
[0.9.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.9.0...hercules-ci-agent-0.9.1
[0.9.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.7...hercules-ci-agent-0.9.0
[0.8.7]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.6...hercules-ci-agent-0.8.7
[0.8.6]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.5...hercules-ci-agent-0.8.6
[0.8.5]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.4...hercules-ci-agent-0.8.5
[0.8.4]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.3...hercules-ci-agent-0.8.4
[0.8.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.2...hercules-ci-agent-0.8.3
[0.8.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.1...hercules-ci-agent-0.8.2
[0.8.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.8.0...hercules-ci-agent-0.8.1
[0.8.0]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.7.5...hercules-ci-agent-0.8.0
[0.7.5]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.7.4...hercules-ci-agent-0.7.5
[0.7.4]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.7.3...hercules-ci-agent-0.7.4
[0.7.3]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.7.2...hercules-ci-agent-0.7.3
[0.7.2]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.7.1...hercules-ci-agent-0.7.2
[0.7.1]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.6...hercules-ci-agent-0.7.1
[0.6.6]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.5...hercules-ci-agent-0.6.6
[0.6.5]: https://github.com/hercules-ci/hercules-ci-agent/compare/hercules-ci-agent-0.6.4...hercules-ci-agent-0.6.5
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
