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
