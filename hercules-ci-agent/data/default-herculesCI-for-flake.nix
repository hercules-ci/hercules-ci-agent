/*
  Helper functions for working with flakes.

  Schema: DefaultHerculesCIHelperSchema
*/
let
  inherit (builtins)
    attrNames
    concatMap
    hasAttr
    intersectAttrs
    listToAttrs
    mapAttrs
    ;

  # lib
  nameValuePair = k: v: { name = k; value = v; };
  filterAttrs = pred: set:
    listToAttrs (
      concatMap
        (name: let v = set.${name}; in if pred name v then [ (nameValuePair name v) ] else [ ])
        (attrNames set)
    );
  optionalAttrs = b: if b then a: a else _: { };
  optionalCall = f: a: if builtins.isFunction f then f a else f;
  # end lib

  # flake -> evalArgs -> { flake | herculesCI }
  addDefaults = flake: evalArgs:
    let
      args = evalArgs // {
        ciSystems =
          if flake?herculesCI.ciSystems
          then listToAttrs (map (sys: nameValuePair sys { }) flake.herculesCI.ciSystems)
          else args.herculesCI.ciSystems;
      };
      herculesCI = optionalCall (flake.herculesCI or { }) evalArgs;
    in
    herculesCI // optionalAttrs (!herculesCI?onPush) {
      onPush.default = {
        outputs = flakeToOutputs flake args;
      };
    };

  flakeToOutputs = flake: args: mapAttrs (translateFlakeAttr args) flake.outputs;

  filterSystems = ciSystems:
    if ciSystems == null
    then attrs: attrs
    else intersectAttrs ciSystems;

  isEnabledSystemConfig = ciSystems:
    if ciSystems == null
    then sys: true
    else
      sys:
      hasAttr sys.config.nixpkgs.localSystem.system ciSystems
      || hasAttr sys.config.nixpkgs.system ciSystems;

  filterSystemConfigs = ciSystems: filterAttrs (k: v:
    if isEnabledSystemConfig ciSystems v
    then true
    else builtins.trace "Ignoring flake attribute ${k} (system not in ciSystems)" false);

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
      forSystems = f: args: attrs: mapAttrs f (filterSystems args.ciSystems attrs);
    in
    rec {
      # TODO validate

      # buildable
      packages = forSystems (sys: pkgs: pkgs);
      checks = forSystems (sys: checks: checks);
      devShell = forSystems (sys: translateShell);
      devShells = forSystems (sys: mapAttrs (k: translateShell));
      apps = forSystems (sys: mapAttrs (k: checkApp));
      nixosConfigurations = args: attrs: mapAttrs (k: sys: { config.system.build.toplevel = sys.config.system.build.toplevel; }) (filterSystemConfigs args.ciSystems attrs);
      darwinConfigurations = args: attrs: mapAttrs (k: sys: { config.system.build.toplevel = sys.config.system.build.toplevel; }) (filterSystemConfigs args.ciSystems attrs);
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
    addDefaults
    flakeToOutputs
    ;
}
