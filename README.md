# Hercules CI Agent

[![Hercules CI](https://hercules-ci.com/api/v1/site/github/account/hercules-ci/project/hercules-ci-agent/badge)](https://hercules-ci.com/github/hercules-ci/hercules-ci-agent/status)

Hercules CI Agent runs on your infrastructure and executes tasks (evaluation, builds, etc)
for your [Hercules CI](https://hercules-ci.com/) account.

## [Documentation](https://docs.hercules-ci.com)

- [Getting Started](https://docs.hercules-ci.com/hercules-ci/getting-started/)
- [Troubleshooting](https://docs.hercules-ci.com/hercules-ci/troubleshooting/)
- [Reference](https://docs.hercules-ci.com/hercules-ci/reference/)

## Hacking

Install into home:

    cabal v2-install --disable-documentation --disable-optimization hercules-ci-agent hercules-ci-cli --overwrite-policy always

Start installed agent

    hercules_ci_agent_bindir=$HOME/.cabal/bin ~/.cabal/bin/hercules-ci-agent

Build `nixUnstable` with cabal:

* change `../nix/shellFor-cabal.nix` to pick the right job from the `ci` param, then
* run:
```
cd hercules-ci-cnix-store-expr
nix-shell ../nix/shellFor-cabal.nix
cabal v2-test -fnix-2_4 --enable-debug-info --disable-library-stripping --disable-executable-stripping
```
