pre-commit@{ config, extendModules, lib, options, ... }:
let
  inherit (lib) intersectLists mkIf mkForce mkOption types;

  hookModule = hook@{ config, ... }: {
    options = {
      customStages = mkOption {
        type = types.listOf types.str;
        default = [ ];
      };
    };
    config = {
      enable =
        mkIf
          (pre-commit.config.customStages != [ ])
          (mkForce (
            intersectLists hook.config.customStages pre-commit.config.customStages != [ ]
          ));
    };
  };

in
{
  options = {
    customStages = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
    withCustomStages = mkOption { internal = true; };
  };
  config = {
    hookModule = hookModule;
    withCustomStages = newStages: (extendModules {
      modules = [{
        customStages =
          lib.mkOverride
            (options.customStages.highestPrio - 1)
            newStages;
      }];
    }).config;
  };
}
