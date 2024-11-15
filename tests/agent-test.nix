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

  # python code is rarely ever changed, but takes like 3s to typecheck
  skipTypeCheck = true;

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
        environment.etc."reference-stdenv".text = builtins.toJSON (pkgs.runCommand "foo"
          {
            nativeBuildInputs = [ pkgs.curl ];
          } "").drvAttrs;
        # It's an offline test, so no caches are available
        nix.settings.substituters = lib.mkForce [ ];
        nix.package = lib.mkIf daemonIsNixUnstable pkgs.nixUnstable;
        boot.kernelModules = [ "loop" ];
        services.hercules-ci-agent.enable = true;
        # Instead of the default, we want the nix library version from the build matrix (which should include at least the default)
        services.hercules-ci-agent.package = lib.mkForce flake.packages.${pkgs.hostPlatform.system}.hercules-ci-agent;

        # test suite fetches tarballs over http:// on the test network.
        services.hercules-ci-agent.settings.allowInsecureBuiltinFetchers = true;

        services.hercules-ci-agent.settings.apiBaseUrl = "http://api";
        services.hercules-ci-agent.settings.binaryCachesPath = (pkgs.writeText "binary-caches.json" (builtins.toJSON { })).outPath;
        services.hercules-ci-agent.settings.clusterJoinTokenPath = (pkgs.writeText "pretend-agent-token" "").outPath;
        # services.hercules-ci-agent.settings.logLevel = "DebugS";
        # services.hercules-ci-agent.settings.nixVerbosity = "debug";
        services.hercules-ci-agent.settings.effectMountables = {
          "forwarded-path" = {
            source = pkgs.runCommand "forwarded-path" { } ''
              mkdir -p $out
              echo "hello from forwarded path" > $out/hello;
            '';
            readOnly = true;
            condition = true;
          };
          "shared-data" = {
            source = "/var/lib/ci-data/shared";
            readOnly = false;
            condition = {
              isRepo = "repo-with-shared-data";
            };
          };
          "hosts" = {
            readOnly = true;
            source = "/etc/hosts";
            condition = true;
          };
          "my-blocks" = {
            readOnly = false;
            # Not using the first device(s), in case the test framework uses it.
            source = "/dev/loop9";
            condition = true;
          };
          "test-condition-type" = {
            readOnly = true;
            source = "/dev/null";
            # bogus expression that contains examples of all possible conditions
            condition = {
              or = [
                { isOwner = "hercules-ci"; }
                { isRepo = "repo-with-shared-data"; }
                { isBranch = "main"; }
                "isTag"
                {
                  and = [
                    "isDefaultBranch"
                    { isBranch = "master"; }
                  ];
                }
              ];
            };
          };
        };

        systemd.services.hercules-ci-agent.serviceConfig.StartLimitBurst = lib.mkForce (agentStartTimeoutSec * 10);
        systemd.services.hercules-ci-agent.serviceConfig.RestartSec = lib.mkForce ("100ms");
        virtualisation.diskSize = 10 * 1024;
        virtualisation.memorySize = 4096;
        virtualisation.cores = 3;
      };
    };
    api = { pkgs, ... }: {
      networking.firewall.allowedTCPPorts = [ 80 ];
      environment.systemPackages = [
        flake.packages.${pkgs.hostPlatform.system}.internal-hercules-ci-agent-test
      ];
      virtualisation.cores = 2;
    };
  };

  testScript =
    ''
      start_all()

      api.wait_for_unit("multi-user.target")
      agent.wait_for_unit("multi-user.target")

      agent.succeed("""
          mkdir -p /var/lib/hercules-ci-agent/secrets
          echo '{}' > /var/lib/hercules-ci-agent/secrets/secrets.json
          chown -R hercules-ci-agent /var/lib/hercules-ci-agent
          chmod 0700 /var/lib/hercules-ci-agent/secrets

          # Custom mount for test
          mkdir -p /var/lib/ci-data/shared
          chown -R hercules-ci-agent /var/lib/ci-data/shared

          # Loopback block device for test
          dd if=/dev/zero of=/root/loop9-backing-file bs=1024 count=1024
          losetup /dev/loop9 /root/loop9-backing-file
          echo "initial data of the loopback block device" > /dev/loop9
          chown hercules-ci-agent /dev/loop9

          # Wait for hercules-ci-agent to fail.
          # This way we know that the agent will come up with a fresh and complete cgroup.
          # This depends on journald being caught up, so we first give it some time to process outdated logs (if any).
          sleep 1
          (journalctl -u hercules-ci-agent.service --follow -n 0 --no-pager JOB_RESULT=failed SYSLOG_IDENTIFIER=systemd || :) | read -n 1

      """)

      # Run the test code + api
      api.succeed(
          """(cd ${testdata} && hercules-ci-agent-test ${runnerArgs} >/dev/console 2>/dev/console)"""
      )
    '';
}
