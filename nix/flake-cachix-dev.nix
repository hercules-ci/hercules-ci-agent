{ lib, config, ... }:
{
  # FIXME: this is incredibly ad hoc; use an extra hercules ci handler with submodules enabled instead (to be implemented)
  #        (alternatively, do something with pre-commit to sync the submodule hash)
  variants.cachix-dev = {
    perSystem = { system, pkgs, ... }: {
      haskellProjects.internal = {
        otherOverlays = lib.mkBefore [
          (self: super:
            let
              flake = builtins.getFlake "git+https://github.com/cachix/cachix.git?rev=498f2fa43de8b2fc2e372c275dece0715101cbda&allRefs=true";
              c = flake.lib.customHaskellPackages { inherit pkgs; haskellPackages = self; };
            in
            # If the overrides change, we need to make sure it's not hercules-ci-cnix-store for example
            assert lib.attrNames c == [ "cachix" "cachix-api" ];
            c)
        ];
      };
    };
  };
  perSystem = { system, ... }:
    # Only build with dev version of cachix on x86_64-linux, as it's mostly about the conditional compilation anyway, and we don't distribute this version.
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.build-with-cachix-1_6 = config.variants.cachix-dev.allSystems.${system}.packages.hercules-ci-agent;
    };
}
