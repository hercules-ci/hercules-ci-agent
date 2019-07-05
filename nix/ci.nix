let
  # nix-build doesn't traverse names with periods...
  targetConfigs = {
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
      system = "x86_64-linux";
    };
    # TODO: fix inline-c-cpp mess
    #"nixos-unstable-darwin" = {
    #  nixpkgsSource = "nixos-unstable";
    #  system = "x86_64-darwin";
    #};
    "nixos-19_03" = {
      nixpkgsSource = "nixos-19.03";
      system = "x86_64-linux";
    };
    "nixos-19_03-darwin" = {
      nixpkgsSource = "nixos-19.03";
      system = "x86_64-darwin";
    };
  };

  recurseIntoAttrs = as: as // { recurseForDerivations = true; };

  targets = builtins.mapAttrs packagesFor targetConfigs;
  
  packagesFor = _targetName: targetConfig:
    import ../default.nix { 
      inherit (targetConfig) nixpkgsSource system;
      allTargets = targets;
    };

in recurseIntoAttrs targets 
