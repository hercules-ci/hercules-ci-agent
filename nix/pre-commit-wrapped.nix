{ lib, config, pkgs, ... }:
let
  inherit (lib)
    getExe
    mkOption
    types
    ;
  json = pkgs.formats.json { };
in
{
  options = {
    wrapped = mkOption {
      type = types.package;
      readOnly = true;
      description = ''
        A wrapped pre-commit package that forces the declared config.
      '';
    };
  };
  config = {
    wrapped = pkgs.writeScriptBin "pre-commit" ''
      #!${pkgs.runtimeShell}
      set -x
      # A command must be specified as the first argument.
      cmd=$1
      shift
      exec ${lib.getExe config.package} \
        $cmd \
        --config ${json.generate "pre-commit-hooks.json" config.rawConfig} \
        "$@"
    '';
  };
}
