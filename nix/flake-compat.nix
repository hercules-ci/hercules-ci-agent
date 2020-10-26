let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  inherit (lock.nodes.flake-compat.locked) owner repo rev;
  flake-compat = builtins.fetchTarball "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
in
import flake-compat { src = ../.; }
