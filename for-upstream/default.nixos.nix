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

  command = "${cfg.package}/bin/hercules-ci-agent --config ${cfg.tomlFile}";
  testCommand = "${command} --test-configuration";

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
      # Allow agent to continue running as long as possible during restart.
      # NOTE: ExecStop commands will be taken from the new configuration,
      #       but we don't currently have any.
      stopIfChanged = false;
      serviceConfig = {
        User = cfg.user;
        ExecStart = command;
        Restart = "on-failure";
        RestartSec = 120;
        StartLimitBurst = 30 * 1000000; # practically infinite
        RuntimeDirectory = "runc";
        RuntimeDirectoryPreserve = "yes";
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
        # Wait a bit, with the effect of bundling up file changes into a single
        # run of this script and hopefully a single restart.
        sleep 10
        if systemctl is-active --quiet hercules-ci-agent.service; then
          if ${testCommand}; then
            systemctl restart hercules-ci-agent.service
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
    nix.trustedUsers = [ cfg.user ];
    services.hercules-ci-agent.extraOptions.nixUserIsTrusted = true;

    users = mkIf (cfg.user == defaultUser) {
      users.hercules-ci-agent =
        if config.ids.uids ? "hercules-ci-agent"
        then { uid = config.ids.uids.hercules-ci-agent; } // defaultUserDetails
        else defaultUserDetails;
    };
  };
}
