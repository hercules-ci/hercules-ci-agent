let

  # nix-build doesn't traverse names with periods...
  targetConfigs = {
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
    };
    "nixos-18_09" = {
      nixpkgsSource = "nixos-18.09";
    };
    "nixos-19_03" = {
      nixpkgsSource = "nixos-19.03";
    };
  };

  recurseIntoAttrs = as: as // { recurseForDerivations = true; };
  
  packagesFor = _targetName: targetConfig:
    let
      pkgs = import ./nix { inherit (targetConfig) nixpkgsSource; };
    in recurseIntoAttrs {
      inherit (pkgs) hercules-ci-agent hercules-ci-agent-packages;
    };

  targets = builtins.mapAttrs packagesFor targetConfigs;
in

recurseIntoAttrs targets // {
  foreachTarget = f: builtins.mapAttrs (_targetName: packages: f packages) targets;
}
