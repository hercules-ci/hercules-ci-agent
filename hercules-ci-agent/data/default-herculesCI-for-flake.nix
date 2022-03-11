/*
  Helper functions for working with flakes.

  Schema: DefaultHerculesCIHelperSchema
*/
let
  inherit (builtins) mapAttrs;

  defaultHerculesCI = flake: args: {
    onPush.default.outputs = flakeToOutputs flake args;
  };

  flakeToOutputs = flake: args: mapAttrs (translateFlakeAttr args) flake.outputs;


  translateFlakeAttr = args: k: v:
    if translations?${k}
    then translations.${k} args v
    else if deprecated?${k}
    then builtins.trace "Ignoring flake attribute ${k} (deprecated)" null
    else builtins.trace "Ignoring flake attribute ${k}" null;

  deprecated = {
    defaultApp = null;
    defaultPackage = null;
    defaultTemplate = null;
    defaultBundler = null;
    overlay = null;
    # too soon?
    # devShell = null;
  };

  checkApp = app:
    if app.type != "app"
    then throw "App type attribute must be set to \"app\"."
    else if builtins.typeOf app.program == "string"
    then {
      program = validateProgramFromStringContext app.program;
    }
    else {
      inherit (app) program;
    };

  validateProgramFromStringContext = s:
    let
      ctx = builtins.getContext s;
      drvs = builtins.attrNames ctx;
      drvPath =
        if drvs == [ ]
        then throw "The provided program string does not have a package in its context. Please set the app's program attribute to a package with `meta.mainProgram` or to a string of the form \"\${pkg}/bin/command\", where `pkg` is a package."
        else if builtins.length drvs != 1
        then throw "The provided program string has multiple packages in its context. Please set the app's program attribute to a single package with `meta.mainProgram` or to a string of the form \"\${pkg}/bin/command\", where `pkg` is a single package."
        else builtins.head drvs;
      basename = baseNameOf drvPath;
      hashLength = 33;
      l = builtins.stringLength basename;
    in
    {
      name = builtins.substring hashLength (l - hashLength - 4) basename;
      type = "derivation";
      drvPath = drvPath;
    };

  translateShell = drv:
    if drv.type or null != "derivation"
    then throw "Attribute is not a shell derivation"
    else drv // { buildDependenciesOnly = true; };

  translations =
    let
      ignore = args: x: null;
      same = args: x: x;
    in
    rec {
      # TODO validate

      # buildable
      packages = same;
      checks = same;
      devShell = args: mapAttrs (k: translateShell);
      devShells = args: mapAttrs (k: mapAttrs (k: translateShell));
      apps = args: mapAttrs (k: mapAttrs (k: checkApp));
      nixosConfigurations = args: mapAttrs (k: sys: { config.system.build.toplevel = sys.config.system.build.toplevel; });
      darwinConfigurations = args: mapAttrs (k: sys: { config.system.build.toplevel = sys.config.system.build.toplevel; });
      effects = args: effects:
        if builtins.isAttrs effects
        then effects
        else effects args;

      # not buildable
      herculesCI = ignore;
      overlays = ignore;
      submodules = ignore;
      nixosModules = ignore;
      darwinModules = ignore;
      legacyPackages = ignore;

    };

in
{
  inherit
    defaultHerculesCI
    ;
}
