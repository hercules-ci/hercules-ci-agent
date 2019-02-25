{ haskellPackages
, haskell
, pkgs
, nix
, ...
}:

let
  haskellPackages_ = haskellPackages;
  inherit (pkgs.lib) cleanSource makeBinPath;
  inherit (haskell.lib) addBuildDepends overrideCabal buildFromSdist;
  internal = rec {
    inherit pkgs;

    # TODO: upstream the overrides
    haskellPackages = haskellPackages_.extend (self: super: {
      hercules-ci-api =
        let basePkg = super.callCabal2nix "hercules-ci-api" (cleanSource ../hercules-ci-api) {};
        in
          buildFromSdist basePkg;

      hercules-ci-agent =
        let basePkg = super.callCabal2nix
                   "hercules-ci-agent"
                   (cleanSource ../hercules-ci-agent)
                   {
                     nix-store = nix;
                     nix-expr = nix;
                     nix-main = nix;
                     bdw-gc = pkgs.boehmgc;
                   };
        in
          buildFromSdist (overrideCabal (
            addBuildDepends
              basePkg
              [ pkgs.makeWrapper pkgs.boost pkgs.boehmgc ]
          ) (o: {
            postInstall = o.postInstall or "" + ''
              wrapProgram $out/bin/hercules-agent --prefix PATH : ${makeBinPath [ pkgs.gnutar pkgs.gzip nix ]}
            '';
            passthru = o.passthru // {
              inherit nix;
            };
          }));

    });

    # Provides system-like dependencies for stack.yaml
    hercules-haskell-shell = haskellPackages.shellFor {
      nativeBuildInputs = [ pkgs.pkgconfig ]; # May want to add some tools here.
      withHoogle = true;
      packages = p: [p.hercules-ci-agent p.hercules-ci-api];
    };

    hercules-ci-api-swagger = pkgs.callPackage ../hercules-ci-api/swagger.nix { inherit (haskellPackages) hercules-ci-api; };
  };
in
pkgs.recurseIntoAttrs {
  inherit (internal.haskellPackages) hercules-ci-agent;
  inherit (internal) hercules-ci-api-swagger;

  # Not traversed for derivations:
  inherit internal;
}
