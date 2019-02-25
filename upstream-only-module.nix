{ pkgs, config, lib, ... }:

let

  cfg = config.services.hercules-ci-agent;
  inherit (lib) mkOption mkIf types escapeShellArg;

  agentUser = {
    name = cfg.user;
    home = "/var/lib/hercules-agent";
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
      default = "hercules-agent";
      type = types.string;
    };
    apiBaseUrl = mkOption {
      description = "Alternative base URL for the Hercules API";
      default = null;
      type = types.nullOr types.string;
    };
    agentTokenPath = mkOption {
      description = "Token for the agent to authenticate with the Hercules API";
      type = types.path;
    };
    concurrentTasks = mkOption {
      description = "Number of tasks to perform simultaneously, such as evaluations, derivations";
      type = types.int;
      default = 1;
    };
    package = mkOption {
      description = "Package containing the bin/hercules-agent program";
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
        ExecStart = "${cfg.package}/bin/hercules-agent ${if (cfg.apiBaseUrl == null) then "" else "--api-base-url ${escapeShellArg cfg.apiBaseUrl}"} --agent-token-path ${escapeShellArg cfg.agentTokenPath} --concurrent-tasks ${toString cfg.concurrentTasks}";
        Restart = "on-failure";
        RestartSec = 120;
        StartLimitBurst = 30 * 1000000; # practically infitine
      };
    };

    # FIXME: Is this the right approach?
    users.extraUsers.hercules-agent =
      if config.ids.uids ? "hercules-agent"
      then { uid = config.ids.uids.hercules-agent; } // agentUser
      else agentUser;

  };
}
