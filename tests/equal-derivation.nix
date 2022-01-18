# See https://github.com/NixOS/nixpkgs/pull/126718
{ lib, runCommand, emptyFile, nix-diff }:

/* Checks that two packages produce the exact same build instructions.
  This can be used to make sure that a certain difference of configuration,
  such as the presence of an overlay does not cause a cache miss.
*/
assertion: a: b:
let
  drvA = builtins.unsafeDiscardOutputDependency a.drvPath or (builtins.trace (builtins.typeOf a) throw "testEqualDerivation second argument must be a package");
  drvB = builtins.unsafeDiscardOutputDependency b.drvPath or (builtins.trace (builtins.typeOf b) throw "testEqualDerivation third argument must be a package");
  name =
    if a?name
    then lib.strings.sanitizeDerivationName "testEqualDerivation-${a.name}"
    else "testEqualDerivation";
in
if drvA == drvB then
  emptyFile
else
  runCommand name
  {
    inherit assertion drvA drvB;
    nativeBuildInputs = [ nix-diff ];
  } ''
    echo "$assertion"
    echo "However, the derivations differ:"
    echo
    echo nix-diff $drvA $drvB
    nix-diff $drvA $drvB
    exit 1
  ''
