{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkIf types filterAttrs;
  inherit (pkgs.callPackage ./to-toml {}) toTOML;

  cfg =
    config.services.hercules-ci-agent;

  checkNix =
    if !cfg.checkNix
    then ""
    else if lib.versionAtLeast config.nix.package.version "2.4.0"
    then ""
    else pkgs.stdenv.mkDerivation {
      name = "hercules-ci-check-system-nix-src";
      inherit (config.nix.package) src patches;
      configurePhase = ":";
      buildPhase = ''
        echo "Checking in-memory pathInfoCache expiry"
        if ! grep 'struct PathInfoCacheValue' src/libstore/store-api.hh >/dev/null; then
          cat 1>&2 <<EOF

          You are deploying Hercules CI Agent on a system with an incompatible
          nix-daemon. Please
           - either upgrade Nix to version 2.4.0 (when released),
           - or set option services.hercules-ci-agent.patchNix = true;
           - or set option nix.package to a build of Nix 2.3 with this patch applied:
               https://github.com/NixOS/nix/pull/3405

          The patch is required for Nix-daemon clients that expect a change in binary
          cache contents while running, like the agent's evaluator. Without it, import
          from derivation will fail if your cluster has more than one machine.
          We are conservative with changes to the overall system, which is why we
          keep changes to a minimum and why we ask for confirmation in the form of
          services.hercules-ci-agent.patchNix = true before applying.

        EOF
          exit 1
        fi
      '';
      installPhase = "echo ok > $out";
    };

  patchedNix = lib.mkIf (!lib.versionAtLeast pkgs.nix.version "2.4.0") (
    if lib.versionAtLeast pkgs.nix.version "2.4pre"
    then lib.warn "Hercules CI Agent module will not patch 2.4 pre-release. Make sure it includes (equivalently) PR #3043, commit d048577909 or is no older than 2020-03-13." pkgs.nix
    else pkgs.nix.overrideAttrs (
      o: {
        patches = (o.patches or []) ++ [ ./issue-3398-path-info-cache-ttls-backport-2.3.patch ];
      }
    )
  );
in
{
  imports = [
    ./gc.nix
  ];

  options.services.hercules-ci-agent = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "If true, run the agent as a system service";
    };
    patchNix = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to set nix.package to a patched pkgs.nix if necessary.

        We strive for minimal patching. This option is only for your convenience,
        when an important patch is not accepted into the stable branch for
        example.
      '';
    };
    checkNix = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to make sure that the system's Nix (nix-daemon) is compatible.

        If you set this to false, please take the responsibility to keep up with
        the change log.
      '';
    };
    baseDirectory = mkOption {
      type = types.path;
      default = "/var/lib/hercules-ci-agent";
      description = "State directory (secrets, work directory, etc) for agent";
    };
    user = mkOption {
      description = "Unix system user that runs the agent service";
      type = types.str;
    };
    package = let
      version = "0.6.6";
    in
      mkOption {
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

    concurrentTasks = mkOption {
      description = "Number of tasks to perform simultaneously, such as evaluations, derivations";
      type = types.int;
      default = 4;
    };

    /*
      Internal and/or computed values
     */
    fileConfig = mkOption {
      type = types.attrsOf types.unspecified;
      readOnly = true;
      internal = true;
      description = ''
        The fully assembled config file as an attribute set, right before it's
        written to file.
      '';
    };
    effectiveConfig = mkOption {
      type = types.attrsOf types.unspecified;
      readOnly = true;
      internal = true;
      description = ''
        The fully assembled config file as an attribute set plus some derived defaults.
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
    nix.extraOptions = lib.addContextFrom checkNix ''
      # A store path that was missing at first may well have finished building,
      # even shortly after the previous lookup. This *also* applies to the daemon.
      narinfo-cache-negative-ttl = 0
    '';
    nix.package = mkIf cfg.patchNix patchedNix;
    services.hercules-ci-agent = {
      secretsDirectory = cfg.baseDirectory + "/secrets";
      tomlFile = pkgs.writeText "hercules-ci-agent.toml"
        (toTOML cfg.fileConfig);

      fileConfig = filterAttrs (k: v: k == "binaryCachesPath" -> v != null) (
        {
          inherit (cfg) concurrentTasks baseDirectory;
        }
        // cfg.extraOptions
      );
      effectiveConfig =
        let
          # recursively compute the effective configuration
          effectiveConfig = defaults // cfg.fileConfig;
          defaults = {
            workDirectory = effectiveConfig.baseDirectory + "/work";
            staticSecretsDirectory = effectiveConfig.baseDirectory + "/secrets";
            clusterJoinTokenPath = effectiveConfig.staticSecretsDirectory + "/cluster-join-token.key";
            binaryCachesPath = effectiveConfig.staticSecretsDirectory + "/binary-caches.json";
          };
        in
          effectiveConfig;
    };
  };
}
