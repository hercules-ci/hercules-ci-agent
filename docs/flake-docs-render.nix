{ self, lib, ... }:
let
  inherit (lib) mkOption concatMap hasPrefix removePrefix;
in
{
  perSystem = { pkgs, ... }:
    let

      filterTransformOptions = { sourceName, sourcePath, baseUrl }:
        let sourcePathStr = toString sourcePath;
        in
        opt:
        let
          declarations = concatMap
            (decl:
              if hasPrefix sourcePathStr (toString decl)
              then
                let subpath = removePrefix sourcePathStr (toString decl);
                in [{ url = baseUrl + subpath; name = sourceName + subpath; }]
              else [ ]
            )
            opt.declarations;
        in
        if declarations == [ ]
        then opt // { visible = false; }
        else opt // { inherit declarations; };

      # This may need some adjustment...
      # E.g. minimal modules: nixpkgs.nixos-lib.evalModules
      nixosEval = pkgs.nixos {
        imports = [
          {
            options._module.args = mkOption {
              visible = false;
            };
          }
          # self.nixosModules.agent-service
          self.nixosModules.multi-agent-service
        ];
      };
      nixosModuleDoc = pkgs.nixosOptionsDoc {
        inherit (nixosEval) options;
        transformOptions = filterTransformOptions {
          sourceName = "hercules-ci-agent";
          baseUrl = "https://github.com/hercules-ci/hercules-ci-agent/blob/master";
          sourcePath = self.outPath;
        };
      };
    in
    {
      packages.generated-antora-files =
        pkgs.runCommand "generated-antora-files"
          {
            passthru.internal = {
              inherit nixosModuleDoc;
            };
            nativeBuildInputs = [ pkgs.pandoc ];
          } ''
          mkdir -p $out/modules/ROOT/partials/options

          pandoc \
            --from=commonmark \
            --to=asciidoc \
            --output=$out/modules/ROOT/partials/options/nixos-options.adoc \
            < ${nixosModuleDoc.optionsCommonMark};
        '';
    };
}
