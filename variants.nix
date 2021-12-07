{ config, lib, extendModules, moduleType, ... }:
let
  inherit (lib)
    mkOption
    types
    ;
  flakeVariants = config.variants;
in
{
  options = {
    variants = mkOption {
      type = types.lazyAttrsOf moduleType;
      default = { };
      description = ''
        Configurations that extend the regular flake configuration.

        You can set any option here, using lib.mkForce if necessary, and
        then read back from the variant using the config module parameter;
        config.variants.<name>.packages for example.
      '';
    };
  };
  config = {
    perSystem = system: { ... }: {
      options = {
        variants = mkOption {
          default = lib.mapAttrs (_: variant: variant.allSystems.${system} or (variant.perSystem system)) flakeVariants;
          readOnly = true;
          description = ''
            A read-only option that reads system-specific attributes from the
            flake-wide variants option.

            In a top-level flake module we have
            (config.perSystem system).variants.<name> == config.variants.<name>.perSystem system
          '';
        };
      };
    };
  };
}
