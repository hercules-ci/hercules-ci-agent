{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkIf types filterAttrs;
  inherit (pkgs.callPackage ./to-toml {}) toTOML;

  cfg =
    config.services.hercules-ci-agent;
in
{
  imports = [
    ./gc.nix
    ./system-caches.common.nix
  ];

  options.services.hercules-ci-agent = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "If true, run the agent as a system service";
    };
    baseDirectory = mkOption {
      type = types.path;
      default = "/var/lib/hercules-ci-agent";
      description = "State directory (secrets, work directory, etc) for agent";
    };
    user = mkOption {
      description = "Unix system user that runs the agent service";
      type = types.string;
    };
    package = let
        version = "0.3.2";
      in mkOption {
      description = "Package containing the bin/hercules-ci-agent program";
      type = types.package;
      default = (import (builtins.fetchTarball "https://github.com/hercules-ci/hercules-ci-agent/archive/hercules-ci-agent-${version}.tar.gz") {}).hercules-ci-agent;
      defaultText = "hercules-ci-agent-${version}";
    };
    extraOptions = mkOption {
      description = ''
        This lets you can add extra options to the agent's config file, in case
        you are using an upstreamed module with a newer version of the package.

        These will override the other options in this module.

        We recommend that you use the other options where possible, because
        extraOptions can not provide a merge function for the contents of the
        fields.
        '';
      type = types.attrsOf types.unspecified;
      default = {};
    };
    binaryCachesFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        A file path to be read at evaluation time, to configure the system's
        cache settings.

        For the format, see https://docs.hercules-ci.com/#binaryCachesPath
      '';
    };

    concurrentTasks = mkOption {
      description = "Number of tasks to perform simultaneously, such as evaluations, derivations";
      type = types.int;
      default = 4;
    };

    /*
      Internal and/or computed values
     */
    finalConfig = mkOption {
      type = types.attrsOf types.unspecified;
      readOnly = true;
      internal = true;
      description = ''
        The fully assembled config file as an attribute set, right before it's
        written to file.
      '';
    };
    tomlFile = mkOption {
      type = types.path;
      readOnly = true;
      internal = true;
      description = ''
        The fully assembled config file.
      '';
    };
    secretsDirectory = mkOption {
      type = types.path;
      readOnly = true;
      internal = true;
      description = ''
        Secrets directory derived from baseDirectory.
      '';
    };
    # TODO: expose all file and directory locations as readOnly options
  };

  config = mkIf cfg.enable {
    services.hercules-ci-agent = {
      secretsDirectory = cfg.baseDirectory + "/secrets";
      tomlFile = pkgs.writeText "hercules-ci-agent.toml"
                                (toTOML cfg.finalConfig);

      finalConfig = filterAttrs (k: v: k == "binaryCachesPath" -> v != null) (
        {
          inherit (cfg) concurrentTasks baseDirectory;
        } // cfg.extraOptions
      );

      # TODO: expose only the (future) main directory as an option and derive
      # all locations from finalConfig.
      extraOptions.clusterJoinTokenPath = lib.mkDefault (cfg.secretsDirectory + "/cluster-join-token.key");
      extraOptions.binaryCachesPath = lib.mkDefault (lib.mapNullable (_f: cfg.secretsDirectory + "/binary-caches.json") cfg.binaryCachesFile);
    };
  };
}
