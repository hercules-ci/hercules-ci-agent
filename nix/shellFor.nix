{ pkgs ? import ./default.nix {}
}:
pkgs.hercules-ci-agent-packages.internal.haskellPackages.shellFor {
  packages = p: [
    p.hercules-ci-api-core
    p.hercules-ci-api
    p.hercules-ci-api-agent
    p.hercules-ci-agent
    p.hercules-ci-agent-test
  ];
  buildInputs = [
    pkgs.boost
    pkgs.nix
  ];
  nativeBuildInputs = [
    pkgs.yaml2json
    pkgs.jq
  ];
}
