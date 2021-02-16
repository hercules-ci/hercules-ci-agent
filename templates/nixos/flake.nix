{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";

  inputs.hercules-ci-agent.url = "github:hercules-ci/hercules-ci-agent/stable";

  outputs = { self, nixpkgs, hercules-ci-agent }: {

    nixosConfigurations.my-host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hardware-configuration.nix
        hercules-ci-agent.nixosModules.agent-profile
        (
          { pkgs, ... }: {
            services.hercules-ci-agent.enable = true;

            # Number of tasks to run simultaneously. A task is a single
            # derivation build or an evaluation. At minimum, you need 2
            # x86_64-linux concurrentTasks in your cluster, to support IFD.
            # This number can be around the CPU core count; lower if memory
            # is the bottleneck.
            # services.hercules-ci-agent.settings.concurrentTasks = "auto";

            # Let 'nixos-version --json' know about the Git revision
            # of this flake.
            system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          }
        )
      ];
    };

  };
}
