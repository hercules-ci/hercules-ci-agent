# Module similar to (flake).agent-service or the NixOS-bundled module, but
# supports running multiple instance of the agent, each with their own files,
# user, etc.

systemArgs@{ pkgs, config, lib, ... }:
let
  inherit (lib) mkIf mkDefault types mkOption;
  inherit (lib.strings) match;
  topConfig = config;
  literalDocBook = lib.literalDocBook or lib.literalExample;
  literalExpression = lib.literalExpression or lib.literalExample;

  submodule = { config, options, name, ... }:
    let
      inherit (import ../settings.nix { inherit pkgs lib; }) format makeSettingsOptions;
      configFile = format.generate "hercules-ci-agent${suffix}.json" config.settings;
      command = "${config.package}/bin/hercules-ci-agent --config ${configFile}";
      testCommand = "${command} --test-configuration";
      suffix = if name == "" then "" else "-${name}";
      user = topConfig.users.users.${config.user};
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
        group = mkOption {
          type = types.str;
          default = "_hercules-ci-agent";
        };
        logFile = mkOption {
          type = types.str;
          default = "/var/log/hci-agent${suffix}.log";
        };
        baseDirectory = mkOption {
          type = types.str;
          default = "/var/lib/hercules-ci-agent${if name == "" then "" else "/${name}"}"; # Almost a `suffix` logic, but yield subdir
        };
      } // makeSettingsOptions { cfg = config; opt = options; };
      config = let cfg = config; in
        {
          settings = {
            _module.args = {
              packageOption = options.package;
              inherit pkgs;
            };
            baseDirectory = cfg.baseDirectory;
            nixUserIsTrusted = true;
            labels =
              let
                mkIfNotNull = x: mkIf (x != null) x;
              in
              {
                darwin.label = topConfig.system.darwinLabel;
                darwin.revision = topConfig.system.darwinRevision;
                darwin.version = topConfig.system.darwinVersion;
                darwin.nix.daemon = topConfig.nix.useDaemon;
                darwin.nix.sandbox = topConfig.nix.settings.sandbox;
              };
          };
          systemConfig = { config, ... }: {
            launchd.daemons."hci-agent${suffix}" = {
              script = "exec ${command}";

              path = [ config.nix.package ];
              environment = {
                NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
              };

              serviceConfig.KeepAlive = true;
              serviceConfig.RunAtLoad = true;
              serviceConfig.StandardErrorPath = cfg.logFile;
              serviceConfig.StandardOutPath = cfg.logFile;
              serviceConfig.GroupName = cfg.user;
              serviceConfig.UserName = cfg.group;
              serviceConfig.WorkingDirectory = cfg.baseDirectory;
              serviceConfig.WatchPaths = [
                cfg.settings.staticSecretsDirectory
              ];
            };

            nix.settings.trusted-users = [ cfg.user ];

            system.activationScripts.preActivation.text = ''
              touch '${cfg.logFile}'
              chown ${toString user.uid}:${toString user.gid} '${cfg.logFile}'
              if ! test -d ${cfg.baseDirectory}; then
                mkdir -p ${cfg.baseDirectory}
                chown ${toString user.uid}:${toString user.gid} ${cfg.baseDirectory}
              fi
            '';
          };
        };
    };

  mergeSub =
    f: lib.mkMerge (map (sub: f (sub.systemConfig systemArgs)) (lib.attrValues config.services.hercules-ci-agents));
in
{
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
        - User: `hercules-ci-agent` (on darwin we use same username)
        - Default base directory: `/var/lib/hercules-ci-agent-''${name}`
      '';
    };
  };

  config = lib.mkMerge [
    {
      nix = mergeSub (c: c.nix);
      launchd = mergeSub (c: c.launchd);
      system = mergeSub (c: c.system);
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
