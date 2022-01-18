{ callPackage, lib, nixos }:
let
  # Set some labels to smoosh "meta" differences
  labels = {
    agent.source = lib.mkForce "dontcare";
    agent.revision = lib.mkForce "deadbeef";
    module = lib.mkForce "dontcareModule";
  };

  singleton =
    (nixos {
      # NB: these tests use the flake and are not sensitive to nix version overrides.
      imports = [ (import ../nix/flake-compat.nix).defaultNix.nixosModules.multi-agent-service ];
      boot.loader.grub.enable = false;
      fileSystems."/".device = "x";
      services.hercules-ci-agents."".settings = { concurrentTasks = 42; inherit labels; };
    }).config.system.build.toplevel;

  reference =
    (nixos {
      imports = [ (import ../nix/flake-compat.nix).defaultNix.nixosModules.agent-service ];
      boot.loader.grub.enable = false;
      fileSystems."/".device = "x";
      services.hercules-ci-agent.enable = true;
      services.hercules-ci-agent.settings = { concurrentTasks = 42; inherit labels; };
    }).config.system.build.toplevel;

  multi =
    (nixos {
      imports = [ (import ../nix/flake-compat.nix).defaultNix.nixosModules.multi-agent-service ];
      boot.loader.grub.enable = false;
      fileSystems."/".device = "x";
      # Test max length (user names are limited)
      services.hercules-ci-agents."a-bcdefghijklmnopqrstuvwxyz".settings = { concurrentTasks = 1; };
      # Test multiple agents don't interfere statically
      services.hercules-ci-agents."b".settings = { concurrentTasks = 2; };
      services.hercules-ci-agents."c".settings = { concurrentTasks = 3; };
    }).config.system.build.toplevel;

  testEqualDerivation = callPackage ./equal-derivation.nix { };

in
lib.recurseIntoAttrs {
  inherit singleton multi reference;
  eq = testEqualDerivation ''services.hercules-ci-agents."".* should produce the same system as services.hercules-ci-agent.*'' reference singleton;
}
