let
  # nix-build doesn't traverse names with periods...
  targetConfigs = {
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
    };
    "nixos-19_03" = {
      nixpkgsSource = "nixos-19.03";
    };
  };

  recurseIntoAttrs = as: as // { recurseForDerivations = true; };

  targets = builtins.mapAttrs packagesFor targetConfigs;
  
  packagesFor = _targetName: targetConfig:
    import ../default.nix { 
      inherit (targetConfig) nixpkgsSource;
      allTargets = targets;
    };

in recurseIntoAttrs targets 
