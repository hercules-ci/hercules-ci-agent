{ ci ? import ./ci.nix
, system ? builtins.currentSystem
, internal ? ci.nixos-unstable-nixUnstable.${system}.internal
, pkgs ? internal.pkgs
}:
internal.haskellPackages.shellFor {
  # Just in case
  NIX_PATH = "${pkgs.path}";

  packages = p: [
    p.hercules-ci-api-core
    p.hercules-ci-api
    p.hercules-ci-api-agent
    p.hercules-ci-agent
    p.hercules-ci-agent-test
    p.hercules-ci-cli
    p.hercules-ci-cnix-expr
    p.hercules-ci-cnix-store
  ];
  buildInputs = [
    pkgs.boost
    pkgs.nlohmann_json

    # cabal: The pkg-config package 'nix-store' version >=2.0 && <2.4 is required
    # but the version installed on the system is version 2.4pre20210601_5985b8b
    (pkgs.nix.overrideAttrs (o: { suffix = ""; version = "2.4"; }))
  ];
  nativeBuildInputs = [
    pkgs.cabal-install
    # pkgs.jq
  ];
}
