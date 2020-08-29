{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";

  # TODO s/flake/stable
  inputs.hercules-ci-agent.url = "github:hercules-ci/hercules-ci-agent/flake";

  outputs = { self, nixpkgs, hercules-ci-agent }: {

    nixosConfigurations.my-host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hardware-configuration.nix
        hercules-ci-agent.nixosModules.recommended
        (
          { pkgs, ... }: {
            services.hercules-ci-agent.enable = true;
            # Number of jobs to run simultaneously
            services.hercules-ci-agent.concurrentTasks = 4;

            # Let 'nixos-version --json' know about the Git revision
            # of this flake.
            system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          }
        )
      ];
    };

  };
}
