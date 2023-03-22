{
  perSystem = { config, pkgs, ... }: {
    checks.stack-yaml-ghc-matches = pkgs.runCommand "stack-yaml-ghc-matches" { } ''
      stack_version="$(grep -oP '(?<=resolver: ghc-)[0-9.]+' ${../stack.yaml})"
      nix_ghc_version="${config.haskellProjects.internal.basePackages.ghc.version}"
      if [ "$stack_version" != "$nix_ghc_version" ]; then
        echo "stack.yaml resolver does not match ghc version"
        echo "  - stack.yaml resolver: $stack_version"
        echo "  - nix ghc version: $nix_ghc_version"
        echo "Update stack.yaml to match the nix ghc version?"
        exit 1
      fi
      touch $out
    '';
  };
}
