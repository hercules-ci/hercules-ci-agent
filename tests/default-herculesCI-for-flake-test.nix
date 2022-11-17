{ lib ? (builtins.getFlake ("git+file://" + toString ../.)).inputs.nixpkgs.lib }:
let lib_ = lib;
in rec {
  lib = lib_;
  default-hci-for-flake = import ../hercules-ci-agent/data/default-herculesCI-for-flake.nix;
  inherit (default-hci-for-flake) flakeToOutputs;
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

  test = done:
    assert outputs1 == flakeOutputs1;
    done;
}
