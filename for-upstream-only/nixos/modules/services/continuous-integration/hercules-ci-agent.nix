{ pkgs, config, lib, ... }:

let

  inherit (lib) mkOption mkIf types escapeShellArg;

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

  options.services.hercules-ci-agent = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "If true, run the agent as a system service";
    };
    user = mkOption {
      description = "Unix system user that runs the agent service";
      default = defaultUser;
      type = types.string;
    };
    apiBaseUrl = mkOption {
      description = "Alternative base URL for the Hercules API";
      default = null;
      type = types.nullOr types.string;
    };
    clusterJoinTokenPath = mkOption {
      description = ''
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
    package = mkOption {
      description = "Package containing the bin/hercules-ci-agent program";
      type = types.package;
      default = pkgs.hercules-ci-agent;
      defaultText = "pkgs.hercules-ci-agent";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.nix.package == cfg.package.nix;
        message = "config.nix.package must match the version of Nix used in hercules-ci-agent. If you are trying to use a different version of Nix, try overriding the pkgs.nix attribute by means of an overlay.";
      }
    ];
    systemd.services.hercules-ci-agent = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/hercules-ci-agent ${if (cfg.apiBaseUrl == null) then "" else "--api-base-url ${escapeShellArg cfg.apiBaseUrl}"} --cluster-join-token-path ${escapeShellArg cfg.clusterJoinTokenPath} --concurrent-tasks ${toString cfg.concurrentTasks}";
        Restart = "on-failure";
        RestartSec = 120;
        StartLimitBurst = 30 * 1000000; # practically infitine
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
