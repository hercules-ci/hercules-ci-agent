# Hercules CI Agent

Hercules CI Agent runs on your infrastructure and executes tasks (evaluation, builds, etc)
for your [Hercules CI](https://hercules-ci.com/) account.

## [Documentation](https://docs.hercules-ci.com)

- [Getting Started](https://docs.hercules-ci.com/hercules-ci/getting-started/)
- [Troubleshooting](https://docs.hercules-ci.com/hercules-ci/troubleshooting/)
- [Reference](https://docs.hercules-ci.com/hercules-ci/reference/)

## Hacking

Live unit tests:

    $ nix-shell
    $ stack --nix test --file-watch

Ghcide: not currently included in shell. Install ghcide for GHC 8.6.5 in your profile.


Live install into home:

    stack install --work-dir .stack-work-for-install --fast --no-haddock --file-watch

Start installed agent

    hercules_ci_agent_bindir=$HOME/.local/bin ~/.local/bin/hercules-ci-agent

`nixUnstable` with `cabal-install`:

    cd hercules-ci-cnix-store-expr
    nix-shell ../nix/shellFor-nixUnstable.nix
    cabal v2-test -fnix-2_4 --enable-debug-info --disable-library-stripping --disable-executable-stripping
