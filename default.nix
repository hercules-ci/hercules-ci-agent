let

  # nix-build doesn't traverse names with periods...
  nixpkgsSources = {
    "nixos-unstable" = "nixos-unstable";
    "nixos-18_09" = "nixos-18.09";
  };

  recurseIntoAttrs = as: as // { recurseForDerivations = true; };
  
  packagesFor = _attrName: nixpkgsSource:
    let
      pkgs = import ./nix { inherit nixpkgsSource; };
    in recurseIntoAttrs {
      inherit (pkgs) hercules-ci-agent hercules-ci-agent-packages;
    };
in

recurseIntoAttrs (builtins.mapAttrs packagesFor nixpkgsSources)
