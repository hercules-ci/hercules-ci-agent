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
    user = mkOption {
      description = "Unix system user that runs the agent service";
      type = types.string;
    };
    package = mkOption {
      description = "Package containing the bin/hercules-ci-agent program";
      type = types.package;
      default = pkgs.hercules-ci-agent;
      defaultText = "pkgs.hercules-ci-agent";
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

    /*
      Options that go into the config file
     */
    # TODO: Remove, use default, point to extraOptions?
    clusterJoinTokenPath = mkOption {
      description = ''
        Important: Avoid putting secrets in the Nix store. Use a string file
        location here and deploy the actual file to that location separately.

        Location of a the cluster join token. It authorizes the agent to add
        itself to the cluster that the token represents.

        This file is only required to be present for the agent's first run. It
        will be ignored after the agent has used the token successfully.
      '';
      type = types.path;
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
    # TODO: expose all file and directory locations as readOnly options
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.nix.package == cfg.package.nix;
        message = "config.nix.package must match the version of Nix used in hercules-ci-agent. If you are trying to use a different version of Nix, try overriding the pkgs.nix attribute by means of an overlay.";
      }
    ];
    services.hercules-ci-agent = {

      tomlFile = pkgs.writeText "hercules-ci-agent.toml"
                                (toTOML cfg.finalConfig);

      finalConfig = filterAttrs (k: v: k == "binaryCachesPath" -> v != null) (
        {
          inherit (cfg) clusterJoinTokenPath concurrentTasks;
        } // cfg.extraOptions
      );

      # TODO: expose only the (future) main directory as an option and derive 
      # all locations from finalConfig.
      clusterJoinTokenPath = lib.mkDefault "/var/lib/hercules-ci-agent/secrets/cluster-join-token.key";
      extraOptions.binaryCachesPath = lib.mkDefault (
        lib.mapNullable (_f: "/var/lib/hercules-ci-agent/secrets/binary-caches.json") cfg.binaryCachesFile
      );

    };
  };
}
