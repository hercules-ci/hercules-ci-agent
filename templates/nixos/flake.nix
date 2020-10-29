{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";

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
            # Number of jobs to run simultaneously
            services.hercules-ci-agent.concurrentTasks = 4;
            # Automatically apply a required patch for stable Nix if necessary.
            services.hercules-ci-agent.patchNix = true;

            # Let 'nixos-version --json' know about the Git revision
            # of this flake.
            system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          }
        )
      ];
    };

  };
}
