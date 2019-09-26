{ pkgs ? import ../nix }:

let
  inherit (pkgs) runCommand stdenv lib nix;
  inherit (pkgs.lib) concatMapStrings attrValues;
  inherit (pkgs.hercules-ci-agent-packages.internal) projectRootSource;
in

runCommand "test-module-nixos" {
  src = projectRootSource;
  buildInputs = [ nix ];
  nixpkgs = pkgs.path;
  paths = pkgs.closureInfo {
    rootPaths = attrValues (import (../../nix/sources.nix));
  };
  nixopsSrc = stdenv.mkDerivation {
    name = "nixops-src";
    inherit (pkgs.nixops) src;
    buildPhase = "cp -r . $out";
    installPhase = ":";
  };
} ''
  export NIX_LOG_DIR=$TMPDIR
  export NIX_STATE_DIR=$TMPDIR
  mkdir home
  export HOME=$PWD/home
  nix-store --load-db < $paths/registration
  echo 1>&2 nixopsSrc: $nixopsSrc

  modules="$src/tests/module-nixos"

  (
    evaluate() {
      (nix-instantiate --eval --expr --strict --json \
        --readonly-mode --option sandbox false \
        "(import $src/tests/module-nixos/test.nix)" \
        --arg nixpkgs "$nixpkgs" \
        --arg modules "$1" \
        >stdout) 2>&1 | tee stderr 1>&2;
    }
    eval_fail() {
      if evaluate "$@" &>/dev/null
      then echo 1>&2 $name: evaluation must fail!
      fi
    }
    running() {
      current_test="$*"
      echo 1>&2 $name: running "$current_test"...
    }
    on_err() {
      cat 1>&2 stderr # redundant except in eval_fail tests
      echo 1>&2 $name: test case "$current_test" failed with above stderr
    }
    trap on_err ERR

    running sanity check
    evaluate "[$modules/configuration-0.nix]"
    mv stdout base-out

    running just import
    evaluate "[$modules/configuration-1-just-import.nix]"
    diff stdout base-out

    running base case
    evaluate "[$modules/configuration-2-base-case.nix]"
    
    running nixops base case
    evaluate "[$modules/configuration-3-nixops-base-case.nix $nixopsSrc/nix/keys.nix]"

    running nixops empty case
    eval_fail "[$modules/configuration-2-base-case.nix $nixopsSrc/nix/keys.nix]"
    grep binary-caches.json stderr >/dev/null
    grep cluster-join-token.key stderr >/dev/null

    running nixops binaryCachesFile migration case
    eval_fail "[$modules/configuration-3-nixops-binaryCachesFile.nix $nixopsSrc/nix/keys.nix]"
    grep 'The option services.hercules-ci-agent.binaryCachesFile has been removed' stderr >/dev/null
    grep 'deployment.keys."binary-caches.json".keyFile' stderr >/dev/null
    grep binary-caches.json stderr >/dev/null

    running nixos binaryCachesFile migration case
    eval_fail "[$modules/configuration-3-nixos-binaryCachesFile.nix]"
    grep 'The option services.hercules-ci-agent.binaryCachesFile has been removed' stderr >/dev/null
    grep 'remove the services.hercules-ci-agent.binaryCachesFile value' stderr >/dev/null
    grep 'make sure you deploy a binary-caches.json file to your agent' stderr >/dev/null

  )

  echo ok >$out
''
