# Replacement for nixosTest on NixOS <= 18.09

pkgs:
let
  inherit (builtins) readDir attrNames filter;
  inherit (pkgs.lib) recursiveUpdate genAttrs mapAttrs;
  inherit (pkgs.lib.strings) hasSuffix removeSuffix;

  nixosPath = pkgs.path + "/nixos";

  makeTest = f: import (nixosPath + "/tests/make-test.nix") f {
    inherit (pkgs) system config;
  };

  #  - Overrides pkgs argument to NixOS test and
  #  - configures nixpkgs.pkgs in all test nodes
  overridePkgs = f: args@{ ... }:
    let
      network = f (args // { inherit pkgs; });
    in
      network
      // {
           nodes = mapAttrs (
             name: module: {
               imports = [ module defaultModule ];
             }
           ) network.nodes;
         };

  defaultModule = { lib, ... }: {
    nixpkgs.pkgs = lib.mkForce pkgs;
    imports = [
      (nixosPath + "/modules/profiles/minimal.nix")
    ];
  };

  /*
     Parameters:
       vmTestSpec     A NixOS VM test, example:

          { ... }:
          { name = "my-test";
            nodes = {
              machine-1 = someNixOSConfiguration;
              machine-2 = ...;
            }
            testScript = ...
          }

     Result:
       derivation that runs the VM test
   */
  vmTest = vmTestSpec: makeTest (overridePkgs vmTestSpec);

in
vmTest
