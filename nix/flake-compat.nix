let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  inherit (lock.nodes.flake-compat.locked) owner repo rev narHash;
  flake-compat = builtins.fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    sha256 = narHash;
  };
  warn = msg: builtins.trace "[1;31mwarning: ${msg}[0m";
  probablyPure = builtins.getEnv "PATH" == "" && builtins.getEnv "NIX_PATH" == "";
  warned =
    if builtins?getFlake && probablyPure
    then
      warn
        ''It seems that you are using Nix in pure evaluation mode and your Nix supports flakes.
        Please use the flake and its attributes instead of importing files by path.''
    else x: x;
in
warned import flake-compat { src = ../.; }
