{ config, lib, flake-parts-lib, withSystem, ... }:
let
  inherit (lib)
    getExe
    mkMerge
    mkBefore
    mkAfter
    ;
  cfg = config.hercules-ci.flake-update;
in
{
  config = {
    perSystem = {
      pre-commit.settings = { ... }: {
        imports = [
          ./pre-commit-custom-stages.nix
          ./pre-commit-wrapped.nix
        ];
      };
    };
    hercules-ci.flake-update.effect.settings =
      withSystem cfg.effect.system (sys@{ config, ... }: {
        git.update.script = mkMerge [
          (mkBefore ''
            git branch -f _hci_start
          '')
          (mkAfter ''
            git branch -f _hci_mostly_done
            echo "Fixing up with pre-commit..."
            if FILTER_BRANCH_SQUELCH_WARNING=1 \
               git filter-branch \
                  --tree-filter '${getExe (sys.config.pre-commit.settings.withCustomStages ["flake-update"]).wrapped} run -a || echo "pre-commit failed, but that is probably ok; continuing."' \
                  _hci_start..HEAD; then
              echo "Here is the diff of flake-update + pre-commit changes:"
              git diff _hci_mostly_done
            else
              echo "git filter-branch failed, but that is ok if there were no changes. Let's check..."
              git diff _hci_mostly_done
              echo "No changes, so we're good."
            fi
          '')
        ];
      });
  };
}
