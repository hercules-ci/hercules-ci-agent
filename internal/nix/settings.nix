# Not a module
{ pkgs, lib }:
let
  inherit (lib)
    types
    literalExpression
    mkOption
    ;
  literalMD = lib.literalMD or (x: lib.literalDocBook "Documentation not rendered. Please upgrade to a newer NixOS with markdown support.");
  mdDoc = lib.mdDoc or (x: "Documentation not rendered. Please upgrade to a newer NixOS with markdown support.");

  format = pkgs.formats.json { };

  settingsModule = { config, packageOption, pkgs, ... }: {
    options = {
      apiBaseUrl = mkOption {
        description = mdDoc ''
          API base URL that the agent will connect to.

          When using Hercules CI Enterprise, set this to the URL where your
          Hercules CI server is reachable.
        '';
        type = types.str;
        default = "https://hercules-ci.com";
      };
      baseDirectory = mkOption {
        type = types.path;
        default = "/var/lib/hercules-ci-agent";
        description = mdDoc ''
          State directory (secrets, work directory, etc) for agent
        '';
      };
      concurrentTasks = mkOption {
        description = mdDoc ''
          Number of tasks to perform simultaneously.

          A task is a single derivation build, an evaluation or an effect run.
          At minimum, you need 2 concurrent tasks for `x86_64-linux`
          in your cluster, to allow for import from derivation.

          `concurrentTasks` can be around the CPU core count or lower if memory is
          the bottleneck.

          The optimal value depends on the resource consumption characteristics of your workload,
          including memory usage and in-task parallelism. This is typically determined empirically.

          When scaling, it is generally better to have a double-size machine than two machines,
          because each split of resources causes inefficiencies; particularly with regards
          to build latency because of extra downloads.
        '';
        type = types.either types.ints.positive (types.enum [ "auto" ]);
        default = "auto";
        defaultText = literalMD ''
          `"auto"`, meaning equal to the number of CPU cores.
        '';
      };
      labels = mkOption {
        description = mdDoc ''
          A key-value map of user data.

          Any Nix type that is representable in JSON is permitted.
        '';
        type = format.type;
        defaultText = literalExpression ''
          {
            agent.source = "..."; # One of "nixpkgs", "flake", "override"
            lib.version = "...";
            pkgs.version = "...";
          }
        '';
      };
      nixUserIsTrusted = mkOption {
        description = mdDoc ''
          Whether hercules-ci-agent is trusted by the nix-daemon. This allows some optimization.
        '';
        type = types.bool;
        default = false;
        internal = true;
      };
      workDirectory = mkOption {
        description = mdDoc ''
          The directory in which temporary subdirectories are created for task state. This includes sources for Nix evaluation.
        '';
        type = types.path;
        default = config.baseDirectory + "/work";
        defaultText = literalExpression ''baseDirectory + "/work"'';
      };
      staticSecretsDirectory = mkOption {
        description = mdDoc ''
          This is the default directory to look for statically configured secrets like `cluster-join-token.key`.

          See also `clusterJoinTokenPath` and `binaryCachesPath` for fine-grained configuration.
        '';
        type = types.path;
        default = config.baseDirectory + "/secrets";
        defaultText = literalExpression ''baseDirectory + "/secrets"'';
      };
      clusterJoinTokenPath = mkOption {
        description = mdDoc ''
          Location of the cluster-join-token.key file.

          You can retrieve the contents of the file when creating a new agent via
          <https://hercules-ci.com/dashboard>.

          As this value is confidential, it should not be in the store, but
          installed using other means, such as agenix, NixOps
          `deployment.keys`, or manual installation.

          The contents of the file are used for authentication between the agent and the API.
        '';
        type = types.path;
        default = config.staticSecretsDirectory + "/cluster-join-token.key";
        defaultText = literalExpression ''staticSecretsDirectory + "/cluster-join-token.key"'';
      };
      binaryCachesPath = mkOption {
        description = mdDoc ''
          Path to a JSON file containing binary cache secret keys.

          As these values are confidential, they should not be in the store, but
          copied over using other means, such as agenix, NixOps
          `deployment.keys`, or manual installation.

          The format is described on <https://docs.hercules-ci.com/hercules-ci-agent/binary-caches-json/>.
        '';
        type = types.path;
        default = config.staticSecretsDirectory + "/binary-caches.json";
        defaultText = literalExpression ''staticSecretsDirectory + "/binary-caches.json"'';
      };
      secretsJsonPath = mkOption {
        description = mdDoc ''
          Path to a JSON file containing secrets for effects.

          As these values are confidential, they should not be in the store, but
          copied over using other means, such as agenix, NixOps
          `deployment.keys`, or manual installation.

          The format is described on <https://docs.hercules-ci.com/hercules-ci-agent/secrets-json/>.
        '';
        type = types.path;
        default = config.staticSecretsDirectory + "/secrets.json";
        defaultText = literalExpression ''staticSecretsDirectory + "/secrets.json"'';
      };
    };
    config = {
      labels = {
        agent.source =
          if packageOption.highestPrio == (lib.modules.mkOptionDefault { }).priority
          then "nixpkgs"
          else lib.mkOptionDefault "override";
        pkgs.version = pkgs.lib.version;
        lib.version = lib.version;
      };
    };
  };

  closedType = opt:
    let any = lib.types.anything;
    in any // {
      merge = loc: defs:
        let r = any.merge loc defs;
        in if r == { }
        then { }
        else
          throw ''
            ${lib.showOption loc}: Encountered unknown settings.
            Make sure the following settings are typed correctly, or if you override the
            package and these are new options, enable ${lib.showOption opt.loc}.
            The unknown settings are:
            ${lib.generators.toPretty {} r}
          '';
    };


  makeSettingsOptions = { cfg, opt }: {
    settings = mkOption {
      description = mdDoc ''
        These settings are written to the `agent.json` file.
      '';
      type = types.submoduleWith {
        modules = [
          settingsModule
          {
            freeformType =
              if cfg.allowUnknownSettings
              then format.type
              else closedType opt.allowUnknownSettings;
          }
        ];
      };
      default = { };
    };
    allowUnknownSettings = mkOption {
      description = mdDoc ''
        Allow unknown settings to be written to the `agent.json` file.

        This is useful for forward compatibility - if you've overridden the package.
      '';
      type = types.bool;
      default = false;
    };
  };
in
{
  inherit format makeSettingsOptions;
}
