toplevel@{ config, inputs, withSystem, ... }:
{
  perSystem = { config, pkgs, ... }: {
    checks.stack-yaml-ghc-matches = pkgs.runCommand "stack-yaml-ghc-matches" { } ''
      stack_version="$(grep -oP '(?<=resolver: ghc-)[0-9.]+' ${../stack.yaml})"
      nix_ghc_version="${config.haskellProjects.internal.basePackages.ghc.version}"
      if [ "$stack_version" != "$nix_ghc_version" ]; then
        echo "stack.yaml resolver does not match ghc version"
        echo "  - stack.yaml resolver: $stack_version"
        echo "  - nix ghc version: $nix_ghc_version"
        echo "Update stack.yaml to match the nix ghc version?"
        exit 1
      fi
      touch $out
    '';

    pre-commit.pkgs = pkgs;
    pre-commit.settings = {
      hooks = {
        # TODO: hlint.enable = true;
        ormolu.enable = true;
        ormolu.excludes = [
          # CPP
          "Hercules/Agent/Build.hs"
          "Hercules/Agent/Cachix.hs"
          "Hercules/Agent/Compat.hs"
          "Hercules/Agent/StoreFFI.hs"
          "Hercules/CNix/Expr.hs" # parse error in quasiquotation
          "Hercules/CNix/Store.hs" # parse error in quasiquotation + CPP
        ];
        shellcheck.enable = true;
        nixpkgs-fmt.enable = true;
        nixpkgs-fmt.excludes = [ "tests/agent-test/testdata/" ];
      };
      excludes = [
        ".*/vendor/.*"
      ];
      settings.ormolu.defaultExtensions = [ "TypeApplications" ];
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
  herculesCI = { config, ... }: {
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
