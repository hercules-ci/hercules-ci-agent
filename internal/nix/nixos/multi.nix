# Module similar to (flake).agent-service or the NixOS-bundled module, but
# supports running multiple instance of the agent, each with their own files,
# user, etc.

systemArgs@{ pkgs, config, lib, ... }:
let
  inherit (lib) mkIf mkDefault types mkOption;
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
      } // makeSettingsOptions { cfg = config; opt = options; };
      config = let cfg = config; in
        {
          settings = {
            _module.args = {
              packageOption = options.package;
              inherit pkgs;
            };
            baseDirectory = "/var/lib/hercules-ci-agent${suffix}";
            nixUserIsTrusted = true;
            labels =
              let
                mkIfNotNull = x: mkIf (x != null) x;
              in
              {
                nixos.configurationRevision = mkIfNotNull systemArgs.config.system.configurationRevision;
                nixos.release = systemArgs.config.system.nixos.release;
                nixos.label = mkIfNotNull systemArgs.config.system.nixos.label;
                nixos.codeName = systemArgs.config.system.nixos.codeName;
                nixos.tags = systemArgs.config.system.nixos.tags;
                nixos.systemName = mkIfNotNull systemArgs.config.system.name;
              };
          };
          systemConfig = { config, ... }: {
            systemd.services."hercules-ci-agent${suffix}" = {
              wantedBy = [ "multi-user.target" ];
              after = [ "network-online.target" ];
              wants = [ "network-online.target" ];
              path = [ config.nix.package ];
              startLimitBurst = 30 * 1000000; # practically infinite
              serviceConfig = {
                User = user;
                ExecStart = command;
                ExecStartPre = testCommand;
                Restart = "on-failure";
                RestartSec = 120;
                # If a worker goes OOM, don't kill the main process. It needs to
                # report the failure and it's unlikely to be part of the problem.
                OOMPolicy = "continue";

                # Work around excessive stack use by libstdc++ regex
                # https://gcc.gnu.org/bugzilla/show_bug.cgi?id=86164
                # A 256 MiB stack allows between 400 KiB and 1.5 MiB file to be matched by ".*".
                LimitSTACK = 256 * 1024 * 1024;

                # Hardening
                CapabilityBoundingSet = "";
                AmbientCapabilities = "";
                DeviceAllow = "";
                ProtectSystem = "full";
                LockPersonality = true;
                PrivateDevices = true;
                PrivateMounts = true;
                PrivateTmp = true;
                PrivateUsers = true;
                ProtectClock = true;
                ProtectHome = true;
                ProtectKernelModules = true;
                RemoveIPC = true;
                RestrictRealtime = true;
                RestrictSUIDSGID = true;
                RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
                SystemCallArchitectures = "native";
                UMask = "077";

                # # Blocks /proc remount in effect container
                # ProtectKernelLogs = true;
                # ProtectKernelTunables = true;
                # # workDirectory may not exist yet.
                # WorkingDirectory = cfg.settings.workDirectory;
              };
            };

            # Changes in the secrets do not affect the unit in any way that would cause
            # a restart, which is currently necessary to reload the secrets.
            systemd.paths."hercules-ci-agent${suffix}-restart-files" = {
              wantedBy = [ "hercules-ci-agent${suffix}.service" ];
              pathConfig = {
                Unit = "hercules-ci-agent${suffix}-restarter.service";
                PathChanged = [ cfg.settings.clusterJoinTokenPath cfg.settings.binaryCachesPath ];
              };
            };
            systemd.services."hercules-ci-agent-restarter${suffix}" = {
              serviceConfig.Type = "oneshot";
              script = ''
                # Wait a bit, with the effect of bundling up file changes into a single
                # run of this script and hopefully a single restart.
                sleep 10
                if systemctl is-active --quiet hercules-ci-agent${suffix}.service; then
                  if ${testCommand}; then
                    systemctl restart hercules-ci-agent${suffix}.service
                  else
                    echo 1>&2 "WARNING: Not restarting agent because config is not valid at this time."
                  fi
                else
                  echo 1>&2 "Not restarting hercules-ci-agent despite config file update, because it is not already active."
                fi
              '';
            };

            # Trusted user allows simplified configuration and better performance
            # when operating in a cluster.
            nix.settings.trusted-users = [ config.systemd.services."hercules-ci-agent${suffix}".serviceConfig.User ];

            users.users.${user} = {
              home = cfg.settings.baseDirectory;
              createHome = true;
              group = user;
              description = "Hercules CI Agent system user${lib.optionalString (name != "") " for ${name}"}";
              isSystemUser = true;
            };

            users.groups.${user} = { };
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
        - User: `hci-''${name}`
        - Default base directory: `/var/lib/hercules-ci-agent-''${name}`
      '';
    };
  };

  config = lib.mkMerge [
    {
      nix = mergeSub (c: c.nix);
      systemd = mergeSub (c: c.systemd);
      users = mergeSub (c: c.users);
    }
    {
      nix.extraOptions = lib.mkIf (config.services.hercules-ci-agents != { }) ''
        # A store path that was missing at first may well have finished building,
        # even shortly after the previous lookup. This *also* applies to the daemon.
        narinfo-cache-negative-ttl = 0
      '';
    }
  ];

  meta.maintainers = [ lib.maintainers.roberth ];
}
