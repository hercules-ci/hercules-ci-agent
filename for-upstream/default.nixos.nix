{ pkgs, config, lib, ... }:

let

  inherit (lib) mkIf mkDefault;

  cfg = config.services.hercules-ci-agent;

  defaultUser = "hercules-ci-agent";
  defaultUserDetails = {
    name = defaultUser;
    home = "/var/lib/hercules-ci-agent";
    description = "System user for the Hercules Agent";
    isSystemUser = true;
    createHome = true;
  };

in
{
  imports = [
    ./common.nix
    ./deploy-keys.nix
  ];

  config = mkIf cfg.enable {

    services.hercules-ci-agent.user = mkDefault defaultUser;
    services.hercules-ci-agent.extraOptions.baseDirectory = lib.mkDefault defaultUserDetails.home;

    systemd.services.hercules-ci-agent = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/hercules-ci-agent --config ${cfg.tomlFile}";
        Restart = "on-failure";
        RestartSec = 120;
        StartLimitBurst = 30 * 1000000; # practically infinite
      };
    };

    systemd.paths.hercules-ci-agent-restart-files = {
      wantedBy = [ "hercules-ci-agent.service" ];
      pathConfig = {
        Unit = "hercules-ci-agent-restarter.service";
        PathChanged = [ cfg.effectiveConfig.clusterJoinTokenPath ] ++ lib.optional (cfg.effectiveConfig ? binaryCachesPath) cfg.effectiveConfig.binaryCachesPath;
      };
    };

    systemd.services.hercules-ci-agent-restarter = {
      serviceConfig.Type = "oneshot";
      script = ''
        if systemctl is-active --quiet hercules-ci-agent.service; then
          systemctl restart hercules-ci-agent.service
        else
          echo 1>&2 "Not restarting hercules-ci-agent despite config file update, because it is not already active."
        fi
      '';
    };

    nix.trustedUsers = [ cfg.user ];

    users = mkIf (cfg.user == defaultUser) {
      users.hercules-ci-agent =
        if config.ids.uids ? "hercules-ci-agent"
        then { uid = config.ids.uids.hercules-ci-agent; } // defaultUserDetails
        else defaultUserDetails;
    };
  };
}
