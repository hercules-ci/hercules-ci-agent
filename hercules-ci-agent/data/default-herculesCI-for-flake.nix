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
    typeOf
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
  # end lib

  # Not quite the same as lib.toFunction, as it will not call a __functor.
  optionalCall = f: a: if builtins.isFunction f then f a else f;

  # flake -> evalArgs -> { flake | herculesCI }
  addDefaults = flake: evalArgs:
    let
      herculesCI = optionalCall (flake.herculesCI or { }) evalArgs;
      args = evalArgs // {
        ciSystems =
          if herculesCI?ciSystems
          then listToAttrs (map (sys: nameValuePair sys { }) herculesCI.ciSystems)
          else args.herculesCI.ciSystems;
      };
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

  getSystemNixDarwin = sys: sys.config.nixpkgs.system;

  getSystemNixOS = sys:
    if sys?options.nixpkgs.hostPlatform && sys.options.nixpkgs.hostPlatform.isDefined
    then sys.config.nixpkgs.buildPlatform.system
    else sys.config.nixpkgs.localSystem.system or sys.config.nixpkgs.system;

  isEnabledSystemConfig = getter: ciSystems:
    if ciSystems == null
    then sys: true
    else
      sys:
      hasAttr (getter sys) ciSystems;

  filterSystemConfigs = getter: ciSystems: filterAttrs (k: v:
    if isEnabledSystemConfig getter ciSystems v
    then true
    else builtins.trace "Ignoring flake attribute ${k} (system ${getter v} not in ciSystems)" false);

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

  hasPrefix = pre: s: builtins.substring 0 (builtins.stringLength pre) s == pre;

  stringIsStorePath = s: hasPrefix builtins.storeDir (dirOf s);

  isPathLike =
    v: (builtins.typeOf v == "path" && stringIsStorePath (toString v))
      || (builtins.typeOf v == "string" && stringIsStorePath v)
      || (v?__toString && isPathLike (v.__toString v))
      || (v?outPath && isPathLike v.outPath);

  checkTemplate = name: template:
    if !builtins.isAttrs template
    then throw "Template `templates.${name}` is not an attribute set."

    else if !template?description
    then throw "Template `templates.${name}` does not have a `description` string attribute."

    else if typeOf template.description != "string"
    then throw "`templates.${name}.description` must be a string, but its type is ${typeOf template.description}."

    else if !template?path
    then throw "Template `templates.${name}` does not have a `path` attribute."

    else if !isPathLike template.path
    then
      if builtins.typeOf template.path == "string" ||
        builtins.typeOf template.path == "path"
      then throw "`templates.${name}.path` must be a path value in the store, or a string representing a store path, or an attribute set coercible to such values (e.g. cleanSource return value). However, the value is ${toString template.path}"
      else throw "`templates.${name}.path` must be a path value in the store, or a string representing a store path, or an attribute set coercible to such values (e.g. cleanSource return value). However, the type is ${builtins.typeOf template.path}"

    else if !(builtins.pathExists template.path)
    then throw "Template path `templates.${name}.path` points to a location that does not exist."

    else template;

  translations =
    let
      ignore = args: x: null;
      same = args: x: x;
      forSystems = f: args: attrs: mapAttrs f (filterSystems args.ciSystems attrs);
    in
    rec {
      # buildable
      packages = forSystems (sys: pkgs: pkgs);
      checks = forSystems (sys: checks: checks);
      devShell = forSystems (sys: translateShell);
      devShells = forSystems (sys: mapAttrs (k: translateShell));
      apps = forSystems (sys: mapAttrs (k: checkApp));
      nixosConfigurations = args: attrs: mapAttrs (k: sys: { config.system.build.toplevel = sys.config.system.build.toplevel; }) (filterSystemConfigs getSystemNixOS args.ciSystems attrs);
      darwinConfigurations = args: attrs: mapAttrs (k: sys: { config.system.build.toplevel = sys.config.system.build.toplevel; }) (filterSystemConfigs getSystemNixDarwin args.ciSystems attrs);
      formatter = forSystems (sys: formatter: formatter);
      templates = args: attrs: mapAttrs checkTemplate attrs;

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
