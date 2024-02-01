{ pkgs, config, lib, ... }:
let

  # TODO (2025): use https://github.com/NixOS/nixpkgs/pull/285612
  doRename = { from, to, visible, warn, use, withPriority ? true, condition ? true }:
    { config, options, ... }:
      with lib;
      let
        fromOpt = getAttrFromPath from options;
        toOf = attrByPath to
          (abort "Renaming error: option `${showOption to}' does not exist.");
        toType = let opt = attrByPath to { } options; in opt.type or (types.submodule { });
      in
      {
        options = setAttrByPath from (mkOption
          {
            inherit visible;
            description = "Alias of {option}`${showOption to}`.";
            apply = x: use (toOf config);
          } // optionalAttrs (toType != null) {
          type = toType;
        });
        config = mkIf condition (mkMerge [
          (optionalAttrs (options ? warnings) {
            warnings = optional (warn && fromOpt.isDefined)
              "The option `${showOption from}' defined in ${showFiles fromOpt.files} has been renamed to `${showOption to}'.";
          })
          (if withPriority
          then mkAliasAndWrapDefsWithPriority (setAttrByPath to) fromOpt
          else mkAliasAndWrapDefinitions (setAttrByPath to) fromOpt)
        ]);
      };

  alias = optPath:
    doRename {
      from = [ "services" "hercules-ci-agent" ] ++ optPath;
      to = [ "services" "hercules-ci-agents" "" ] ++ optPath;
      visible = true;
      warn = false;
      use = x: x;
      withPriority = false;
      condition = config.services.hercules-ci-agent.enable;
    };
in
{
  imports = [
    ./multi.nix
    (lib.mkRenamedOptionModule [ "services" "hercules-ci-agent" "user" ] [ "systemd" "services" "hercules-ci-agent" "serviceConfig" "User" ])
    (alias [ "package" ])
    (alias [ "settings" ])
    ({ config, ... }: {
      options.services.hercules-ci-agent.enable = lib.mkEnableOption ''`hercules-ci-agents.""` - the default agent.'';
      config = lib.mkIf config.services.hercules-ci-agent.enable {
        services.hercules-ci-agents."" = { };
      };
    })
  ];

  meta.maintainers = [ lib.maintainers.roberth ];
}
