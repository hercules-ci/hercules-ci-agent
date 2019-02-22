let

  # nix-build doesn't traverse names with periods...
  nixpkgsSources = {
    "nixos-unstable" = "nixos-unstable";
    "nixos-18_09" = "nixos-18.09";
  };

  packagesFor = _attrName: nixpkgsSource:
    let
      pkgs = import ./nix { inherit nixpkgsSource; };
    in
      pkgs.packages;
in

{ recurseForDerivations = true;
} // builtins.mapAttrs packagesFor nixpkgsSources
