{ lib ? (builtins.getFlake ("git+file://" + toString ../.)).inputs.nixpkgs.lib }:
let lib_ = lib;
in rec {
  lib = lib_;
  default-hci-for-flake = import ../hercules-ci-agent/data/default-herculesCI-for-flake.nix;

  fakePkg = name: { type = "derivation"; inherit name; outPath = "/path/to/${name}"; };

  inherit (default-hci-for-flake) flakeToOutputs addDefaults;
  flakeToOutputs' = flakeOutputs:
    flakeToOutputs flakeOutputs { ciSystems = { "riscv-freebsd" = { }; }; };
  thisFileDir = builtins.path {
    name = "source";
    path = ./.;
    filter = path: type: toString path == toString ./. || toString path == toString ./this-file.nix;
  };
  pathValueInStoreFromPathExpr = import (thisFileDir + "/this-file.nix");

  flakeOutputs1 = {
    templates = {
      foo = {
        description = "";
        path = pathValueInStoreFromPathExpr;
      };
      foo2 = {
        description = "";
        path = "${pathValueInStoreFromPathExpr}";
      };
      bar = {
        description = "";
        path = lib.cleanSource ./.;
      };
      baz = {
        description = "aaaaaaaaa";
        path.outPath.outPath.outPath = lib.cleanSource ./.;
      };
      qux = {
        description = "";
        path.outPath.__toString = x: lib.cleanSource ./.;
      };
      quu = {
        description = "";
        path.outPath.__toString = x: pathValueInStoreFromPathExpr;
      };
    };
  };
  outputs1 = flakeToOutputs' { outputs = flakeOutputs1; };

  toFlake = x: x // { outputs = x; };
  flake2 = toFlake {
    packages.x86_64-linux.hello = fakePkg "hello86";
    packages.aarch64-darwin.hello = fakePkg "iHello";
    herculesCI = args: {
      ciSystems = [ "x86_64-linux" ];
    };
  };
  outputs2 = addDefaults flake2 { herculesCI = { ciSystems = { "aarch64-darwin" = null; }; }; };

  test = done:
    assert outputs1 == flakeOutputs1;
    assert lib.attrNames (outputs2.onPush.default.outputs.packages) == [ "x86_64-linux" ];
    done;

}
