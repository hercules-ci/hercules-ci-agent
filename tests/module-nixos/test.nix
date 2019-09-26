{ nixpkgs, modules }:

# this runs in the sandbox, so the impurities of nixos glue are not a problem
(
  import (nixpkgs + "/nixos/lib/eval-config.nix") {
    inherit modules;
  }
).config.system.build.toplevel
