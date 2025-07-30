toplevel@{ withSystem, ... }:
{
  # cachix dev version build currently doesn't eval; skip it
  # imports = [ ./flake-cachix-dev.nix ];

  perSystem = { config, pkgs, ... }: {
    pre-commit.pkgs = pkgs;
    pre-commit.settings = {
      hooks = {
        # TODO: hlint.enable = true;
        ormolu.enable = true;
        ormolu.excludes = [
          # If ormolu's CPP support becomes difficult to handle again, add file paths here,
          # or use its magic comments (see its README).
        ];
        shellcheck.enable = true;
        nixpkgs-fmt.enable = true;
        nixpkgs-fmt.excludes = [ "tests/agent-test/testdata/" ];
      };
      excludes = [
        ".*/vendor/.*"
      ];
      hooks.ormolu.settings.defaultExtensions = [ "TypeApplications" ];
    };

  };
  hercules-ci.flake-update = {
    enable = true;
    autoMergeMethod = "merge";
    when = {
      hour = 4;
      dayOfWeek = "Wed";
    };
    flakes = {
      "." = { };
      "dev/private" = { };
    };
  };
  herculesCI = { config, lib, ... }: {
    onPush.default = {
      # Some things aren't essential, so we don't check them.
      # Consider re-enabling these if they're fixed.
      outputs.devShells.aarch64-linux.default = lib.mkOverride 0 { };
      outputs.devShells.aarch64-linux.internal = lib.mkOverride 0 { };
    };

    onSchedule."nix-updates" = {
      when = {
        hour = 4;
        dayOfWeek = "Thu";
      };
      outputs.effects.update = withSystem toplevel.config.defaultEffectSystem ({ hci-effects, pkgs, ... }:
        hci-effects.modularEffect {
          imports = [
            hci-effects.modules.git-update
          ];
          inputs = [ pkgs.nix ];
          secretsMap.token = { type = "GitToken"; };
          git.checkout.remote.url = config.repo.remoteHttpUrl;
          git.checkout.forgeType = config.repo.forgeType;
          git.update.branch = "nix-updates";
          git.update.pullRequest.enable = false;
          git.update.script = ''
            (
              set -x
              git merge origin/master
              nix flake lock --update-input nix \
                --commit-lock-file \
                --extra-experimental-features 'nix-command flakes'
            )
          '';
        }
      );
    };
  };
}
