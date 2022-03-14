# This only exists because nix repl isn't really flake-aware yet

let self = builtins.getFlake ("git+file://" + toString ./.);
in self // { inherit self; }
