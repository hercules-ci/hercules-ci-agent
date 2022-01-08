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
      boot.loader.grub.enable = false;
      fileSystems."/".device = "x";
      services.hercules-ci-agents."".settings = { concurrentTasks = 42; inherit labels; };
      imports = [ ../internal/nix/nixos/multi.nix ];
    }).config.system.build.toplevel;

  reference =
    (nixos {
      boot.loader.grub.enable = false;
      fileSystems."/".device = "x";
      services.hercules-ci-agent.enable = true;
      services.hercules-ci-agent.settings = { concurrentTasks = 42; inherit labels; };
      imports = [ (import ../nix/flake-compat.nix).defaultNix.nixosModules.agent-service ];
    }).config.system.build.toplevel;

  multi =
    (nixos {
      boot.loader.grub.enable = false;
      fileSystems."/".device = "x";
      services.hercules-ci-agents."a".settings = { concurrentTasks = 1; };
      services.hercules-ci-agents."b".settings = { concurrentTasks = 2; };
      services.hercules-ci-agents."c".settings = { concurrentTasks = 3; };
      imports = [ ../internal/nix/nixos/multi.nix ];
    }).config.system.build.toplevel;

  testEqualDerivation = callPackage ./equal-derivation.nix { };

in
lib.recurseIntoAttrs {
  inherit singleton multi reference;
  eq = testEqualDerivation ''services.hercules-ci-agents."".* should produce the same system as services.hercules-ci-agent.*'' reference singleton;
}
