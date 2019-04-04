with { pkgs = import ./nix {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.devTools.niv ];
  }
