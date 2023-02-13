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
    nixosConfigurations.rusty = lib.nixosSystem { modules = [ ]; system = "x86_64-linux"; };
    nixosConfigurations.trusty = lib.nixosSystem { modules = [{ nixpkgs.hostPlatform = "x86_64-linux"; }]; };
    nixosConfigurations.dusty = lib.nixosSystem { modules = [{ nixpkgs.hostPlatform = "i686-linux"; }]; };
    darwinConfigurations.my-intel.config.system.build.toplevel = fakePkg "my-intel" // { stdenv.buildPlatform.system = "x86_64-darwin"; };
    darwinConfigurations.my-apple.config.system.build.toplevel = fakePkg "my-apple" // { stdenv.buildPlatform.system = "aarch64-darwin"; };
    herculesCI = args: {
      ciSystems = [ "x86_64-linux" "x86_64-darwin" ];
    };
  };
  outputs2 = addDefaults flake2 { herculesCI = { ciSystems = { "aarch64-darwin" = null; }; }; };

  test = done:
    assert outputs1 == flakeOutputs1;
    assert lib.attrNames (outputs2.onPush.default.outputs.packages) == [ "x86_64-linux" ];
    assert lib.attrNames (outputs2.onPush.default.outputs.nixosConfigurations) == [ "rusty" "trusty" ];
    assert lib.attrNames (outputs2.onPush.default.outputs.darwinConfigurations) == [ "my-intel" ];
    done;

}
