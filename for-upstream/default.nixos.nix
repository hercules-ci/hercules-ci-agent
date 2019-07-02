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
    ./system-caches.nixos.nix
    ./deploy-keys.nix
  ];

  config = mkIf cfg.enable {

    services.hercules-ci-agent.user = mkDefault defaultUser;
    services.hercules-ci-agent.extraOptions.baseDirectory = lib.mkDefault defaultUserDetails.home;

    systemd.services.hercules-ci-agent = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/hercules-ci-agent --config ${cfg.tomlFile}";
        Restart = "on-failure";
        RestartSec = 120;
        StartLimitBurst = 30 * 1000000; # practically infinite
      };
    };

    systemd.paths.hercules-ci-agent = {
      wantedBy = [ "herucles-ci-agent.service" ];
      pathConfig = {
        PathChanged = [ cfg.secretsDirectory ];
      };
    };

    users = mkIf (cfg.user == defaultUser) {
      users.hercules-ci-agent =
        if config.ids.uids ? "hercules-ci-agent"
        then { uid = config.ids.uids.hercules-ci-agent; } // defaultUserDetails
        else defaultUserDetails;
    };
  };
}
