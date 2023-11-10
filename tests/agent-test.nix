{ flake, daemonIsNixUnstable, trusted ? true }:
{ pkgs, ... }:
let
  testdata = pkgs.runCommand "testdata" { } ''
    mkdir -p $out/testdata
    for p in ${./agent-test/testdata}/*; do
      ln -s $p $out/testdata/$(basename $p);
    done
    ln -s ${pkgs.callPackage ./nixpkgsball.nix { }} $out/testdata/nixpkgs
  '';

  agentStartTimeoutSec = 5 * 60;

  # e.g. --match 'something'
  # runnerArgs = '' --match 'can refer to nixpkgs' '';
  runnerArgs = '''';

  inherit (pkgs.lib) optionalString;

in
{
  name = "agent-test${optionalString daemonIsNixUnstable "-daemon-nixUnstable"}";

  nodes = {
    agent = { config, pkgs, lib, ... }: {
      imports = [
        flake.nixosModules.agent-profile
        {
          config =
            if trusted
            then {
              assertions = [
                {
                  assertion = config.services.hercules-ci-agent.settings.nixUserIsTrusted;
                  message = "nixUserIsTrusted is the default.";
                }
              ];
            }
            else {
              services.hercules-ci-agent.settings.nixUserIsTrusted = lib.mkForce false;
              nix.settings.trusted-users = lib.mkForce [ ];
            };
        }
      ];
      config = {
        # Keep build dependencies around, because we'll be offline
        environment.etc."reference-stdenv".text = builtins.toJSON (pkgs.runCommand "foo" { } "").drvAttrs;
        # It's an offline test, so no caches are available
        nix.settings.substituters = lib.mkForce [ ];
        nix.package = lib.mkIf daemonIsNixUnstable pkgs.nixUnstable;
        services.hercules-ci-agent.enable = true;
        # Instead of the default, we want the nix library version from the build matrix (which should include at least the default)
        services.hercules-ci-agent.package = lib.mkForce flake.packages.${pkgs.hostPlatform.system}.hercules-ci-agent;

        # test suite fetches tarballs over http:// on the test network.
        services.hercules-ci-agent.settings.allowInsecureBuiltinFetchers = true;

        services.hercules-ci-agent.settings.apiBaseUrl = "http://api";
        services.hercules-ci-agent.settings.binaryCachesPath = (pkgs.writeText "binary-caches.json" (builtins.toJSON { })).outPath;
        services.hercules-ci-agent.settings.clusterJoinTokenPath = (pkgs.writeText "pretend-agent-token" "").outPath;
        services.hercules-ci-agent.settings.concurrentTasks = 4; # Decrease on itest memory problems

        systemd.services.hercules-ci-agent.serviceConfig.StartLimitBurst = lib.mkForce (agentStartTimeoutSec * 10);
        systemd.services.hercules-ci-agent.serviceConfig.RestartSec = lib.mkForce ("100ms");
        virtualisation.diskSize = 10 * 1024;
        virtualisation.memorySize = 1024;
      };
    };
    api = { pkgs, ... }: {
      networking.firewall.allowedTCPPorts = [ 80 ];
      environment.systemPackages = [
        flake.packages.${pkgs.hostPlatform.system}.internal-hercules-ci-agent-test
      ];
    };
  };

  testScript =
    ''
      start_all()

      agent.succeed("""
          mkdir -p /var/lib/hercules-ci-agent/secrets
          echo '{}' > /var/lib/hercules-ci-agent/secrets/secrets.json
          chown -R hercules-ci-agent /var/lib/hercules-ci-agent
          chmod 0700 /var/lib/hercules-ci-agent/secrets
      """)

      # Run the test code + api
      api.succeed(
          """(cd ${testdata} && hercules-ci-agent-test ${runnerArgs} >/dev/console 2>/dev/console)"""
      )
    '';
}
