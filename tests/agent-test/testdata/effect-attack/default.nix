{
  herculesCI.onPush.default = {
    inputs = abort "bypassed in test";
    outputs = { nixpkgs ? null }: 
      let
        pkgs = import nixpkgs {};
        effect-1 = pkgs.runCommand "effect-1" { isEffect = true; } ''exit 1;'';
        effect-2 = pkgs.runCommand "effect-2" { isEffect = true; } ''exit 1;'';
      in
      {
        illegal = effect-1;
        effects.ok = effect-2;
      };
  };
}
