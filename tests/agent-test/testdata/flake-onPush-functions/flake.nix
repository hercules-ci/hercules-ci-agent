{
  inputs = { };
  outputs = { self, ... }: {
    herculesCI = args@{ primaryRepo, ref, ... }:
      assert primaryRepo.outPath == self.outPath;
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
  };
}
