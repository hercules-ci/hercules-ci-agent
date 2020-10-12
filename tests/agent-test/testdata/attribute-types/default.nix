let
  base = derivation {
    name = "myPackage";
    builder = "foo";
    system = "x86_64-linux";
  };
in {
  deps-only = base // { buildDependenciesOnly = true; };
  effect = base // { isEffect = true; };
  ignore-fail = base // { ignoreFailure = true; };
  regular = base;
  require-fail = base // { requireFailure = true; };
  shell = base // { phases = ["nobuildPhase"]; };
}
