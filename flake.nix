{
  description = "Hercules CI Agent";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";

  outputs = { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      filterMeta = nixpkgs.lib.filterAttrs (k: v: k != "meta" && k != "recurseForDerivations");
    in
      {

        packages =
          nixpkgs.lib.mapAttrs
            (
              k: v:
                { inherit (filterMeta v) hercules-ci-agent; }
            )
            (filterMeta (import ./nix/ci.nix).nixos-20_03);

        # This may change
        # defaultPackage =
        #   nixpkgs.lib.mapAttrs (k: p: p.hercules-ci-agent) (
        #     nixpkgs.lib.filterAttrs (k: v: k != "meta") self.packages.nixos-20_03
        #   );

        # nixosModules.bare = { imports = [ ./....nix ]; }; # TODO
        nixosModules.recommended = { imports = [ ./module.nix ]; };

        # defaultApp: reserved for a CLI command

        defaultTemplate = self.templates.nixos;
        templates = {
          nixos = {
            path = ./templates/nixos;
            description = "A NixOS configuration with Hercules CI Agent";
          };
        };
      };
}
