{
  herculesCI = args@{ primaryRepo, ref, ... }:
    assert args.branch == "main";
    assert primaryRepo.branch == "main";
    assert args.ref == "refs/heads/main";
    assert primaryRepo.ref == "refs/heads/main";
    assert args.rev == "eefe2e4df3a0f147cf0f59438010b63fd857291b";
    assert primaryRepo.rev == "eefe2e4df3a0f147cf0f59438010b63fd857291b";
    assert args.shortRev == "eefe2e4";
    assert primaryRepo.shortRev == "eefe2e4";
    assert args.tag == null;
    assert primaryRepo.tag == null;
    # Non-flake checkouts must not be added to the store; at least not by agent itself
    # not a store path:
    assert builtins.substring 0 (builtins.stringLength builtins.storeDir) primaryRepo.outPath != builtins.storeDir;
    # contains a ci.nix
    assert (builtins.readDir primaryRepo.outPath)."ci.nix" == "regular";
    # contains this ci.nix
    assert toString ./. == primaryRepo.outPath;
    {
      onPush.default.outputs = { ... }: {
        packages.x86_64-linux.default = derivation {
          name = "default-package";
          builder = "foo";
          system = "x86_64-linux";
        };
      };
      packages = throw "don't use flake.packages!";
    };
  honeypot = throw "don't use the other attrs!";
}

