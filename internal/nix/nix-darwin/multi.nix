# Module similar to (flake).agent-service or the NixOS-bundled module, but
# supports running multiple instance of the agent, each with their own files,
# user, etc.

systemArgs@{ pkgs, config, lib, ... }:
let
  inherit (lib) mkIf mkDefault types mkOption;
  inherit (lib.strings) match;
  literalDocBook = lib.literalDocBook or lib.literalExample;
  literalExpression = lib.literalExpression or lib.literalExample;

  submodule = { config, options, name, ... }:
    let
      inherit (import ../settings.nix { inherit pkgs lib; }) format makeSettingsOptions;
      configFile = format.generate "hercules-ci-agent${suffix}.json" config.settings;
      command = "${config.package}/bin/hercules-ci-agent --config ${configFile}";
      testCommand = "${command} --test-configuration";
      suffix = if name == "" then "" else "-${name}";
      user = if name == "" then "hercules-ci-agent" else "hci-${name}";
    in
    {
      options = {
        systemConfig = lib.mkOption {
          internal = true;
          type = types.unspecified; # A function from module arguments to config.
        };
        package = mkOption {
          description = ''
            Package containing the bin/hercules-ci-agent executable.
          '';
          type = types.package;
          default = pkgs.hercules-ci-agent;
          defaultText = literalExpression "pkgs.hercules-ci-agent";
        };
        user = mkOption {
          type = types.str;
          default = "_hercules-ci-agent";
        };
      } // makeSettingsOptions { cfg = config; opt = options; };
      config = let cfg = config; in
        {
          settings = {
            _module.args = {
              packageOption = options.package;
              inherit pkgs;
            };
            baseDirectory = "/var/lib/hercules-ci-agent${if name == "" then "" else "/${name}"}"; # Almost a `suffix` logic, but yield subdir
            nixUserIsTrusted = true;
            labels =
              let
                mkIfNotNull = x: mkIf (x != null) x;
              in
              {
                darwin.label = config.system.darwinLabel;
                darwin.revision = config.system.darwinRevision;
                darwin.version = config.system.darwinVersion;
                darwin.nix.daemon = config.nix.useDaemon;
                darwin.nix.sandbox = config.nix.settings.sandbox;
              };
          };
          systemConfig = { config, ... }: {
            launchd.daemons.hercules-ci-agent = {
              script = "exec ${cfg.package}/bin/hercules-ci-agent --config ${cfg.jsonFile}";

              path = [ config.nix.package ];
              environment = {
                NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
              };

              serviceConfig.KeepAlive = true;
              serviceConfig.RunAtLoad = true;
              serviceConfig.StandardErrorPath = cfg.logFile;
              serviceConfig.StandardOutPath = cfg.logFile;
              serviceConfig.GroupName = "_hercules-ci-agent";
              serviceConfig.UserName = "_hercules-ci-agent";
              serviceConfig.WorkingDirectory = user.home;
              serviceConfig.WatchPaths = [
                cfg.settings.staticSecretsDirectory
              ];
            };

            system.activationScripts.preActivation.text = ''
              touch '${cfg.logFile}'
              chown ${toString user.uid}:${toString user.gid} '${cfg.logFile}'
            '';
            # Trusted user allows simplified configuration and better performance
            # when operating in a cluster.
            nix.settings.trusted-users = [ config.systemd.services."hercules-ci-agent${suffix}".serviceConfig.User ];
          };
        };
    };

  mergeSub =
    f: lib.mkMerge (map (sub: f (sub.systemConfig systemArgs)) (lib.attrValues config.services.hercules-ci-agents));
in {
  options = {
    services.hercules-ci-agents = lib.mkOption {
      type = types.attrsOf (types.submoduleWith {
        modules = [ submodule ];
      });
      default = { };
      description = ''
        Multiple instances of hercules-ci-agent can be specified.

        If you specify an instance named `""`, it will behave just as the `services.hercules-ci-agent` options did.
        - User: `hercules-ci-agent`
        - Default base directory: `/var/lib/hercules-ci-agent`

        Otherwise:
        - User: `hci-''${name}`
        - Default base directory: `/var/lib/hercules-ci-agent-''${name}`
      '';
    };
  };

  config = lib.mkMerge [
    {
      nix = mergeSub (c: c.nix);
      launchd = mergeSub (c: c.launchd);
# FIXME: no per-agent user support      
#      users = mergeSub (c: c.users);
    }
    {
      nix.extraOptions = lib.mkIf (config.services.hercules-ci-agents != { }) ''
        # A store path that was missing at first may well have finished building,
        # even shortly after the previous lookup. This *also* applies to the daemon.
        narinfo-cache-negative-ttl = 0
      '';

      users.knownGroups = [ "hercules-ci-agent" "_hercules-ci-agent" ];
      users.knownUsers = [ "hercules-ci-agent" "_hercules-ci-agent" ];

      users.users._hercules-ci-agent = {
        uid = mkDefault 399;
        gid = mkDefault config.users.groups._hercules-ci-agent.gid;
        home = mkDefault "/var/lib/hercules-ci-agent";
        name = "_hercules-ci-agent";
        createHome = true;
        shell = "/bin/bash";
        description = "System user for the Hercules CI Agent";
      };
      users.groups._hercules-ci-agent = {
        gid = mkDefault 32001;
        name = "_hercules-ci-agent";
        description = "System group for the Hercules CI Agent";
      };
    }
  ];

  meta.maintainers = [ ];
}
