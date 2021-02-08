{ pkgs, ... }:
let
  tarball = x: pkgs.runCommand "${x.name or "tarball"}.tar.gz"
    {
      inherit x;
    } ''
    cd $x && tar -c . | gzip -1 >$out
  '';

  testdata = pkgs.runCommand "testdata" { } ''
    mkdir -p $out/testdata
    for p in ${./agent-test/testdata}/*; do
      ln -s $p $out/testdata/$(basename $p);
    done
    ln -s ${tarball pkgs.path} $out/testdata/nixpkgs
  '';

  agentStartTimeoutSec = 5 * 60;

  runnerArgs = '''';

in
{
  name = "agent-test";

  nodes = {
    agent = { config, pkgs, lib, ... }: {
      imports = [
        ../module.nix
      ];
      config = {
        # Keep build dependencies around, because we'll be offline
        environment.etc."reference-stdenv".text = builtins.toJSON (pkgs.runCommand "foo" { } "").drvAttrs;
        # It's an offline test, so no caches are available
        nix.binaryCaches = lib.mkForce [ ];
        services.hercules-ci-agent.enable = true;
        services.hercules-ci-agent.settings.apiBaseUrl = "http://api";
        services.hercules-ci-agent.settings.nixUserIsTrusted = lib.mkForce false;
        services.hercules-ci-agent.settings.binaryCachesPath = (pkgs.writeText "binary-caches.json" (builtins.toJSON { })).outPath;
        services.hercules-ci-agent.settings.clusterJoinTokenPath = (pkgs.writeText "pretend-agent-token" "").outPath;
        services.hercules-ci-agent.settings.concurrentTasks = 4; # Decrease on itest memory problems

        systemd.services.hercules-ci-agent.serviceConfig.StartLimitBurst = lib.mkForce (agentStartTimeoutSec * 10);
        systemd.services.hercules-ci-agent.serviceConfig.RestartSec = lib.mkForce ("100ms");
        virtualisation.diskSize = 6 * 1024;
      };
    };
    api = { ... }: {
      networking.firewall.allowedTCPPorts = [ 80 ];
      environment.systemPackages = [ pkgs.testSuitePkgs.hercules-ci-agent-packages.internal.haskellPackages.hercules-ci-agent-test ];
    };
  };

  testScript =
    ''
      start_all()

      # Run the test code + api
      api.succeed(
          """(cd ${testdata} && hercules-ci-agent-test ${runnerArgs} >/dev/console 2>/dev/console)"""
      )
    '';
}
