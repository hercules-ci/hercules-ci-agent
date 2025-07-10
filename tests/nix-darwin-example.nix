{ flake ? (import ../nix/flake-compat.nix).defaultNix, pkgs }:
let
  nix-darwin = flake.inputs.nix-darwin;
  inherit (pkgs) system;
  configuration = { extendModules, lib, hypothetical, ... }: {
    imports = [
      flake.darwinModules.agent-profile
    ];
    system.stateVersion = 6;
    services.hercules-ci-agent.enable = true;
    services.hercules-ci-agent.settings.labels.testNull = null;

    # Check that the check on the "trusted-users =" setting works
    _module.args.hypothetical = lib.mkDefault false;
    assertions = lib.mkIf (!hypothetical) [
      {
        assertion =
          let
            c = extendModules {
              modules = [
                {
                  _module.args.hypothetical = true;
                  nix.extraOptions = ''
                    trusted-users = root @wheel
                  '';
                }
              ];
            };
            getMessages = x: map (assertion: assertion.message) (lib.filter (assertion: !assertion.assertion) x);
            eq = actual: expected:
              if actual == expected
              then true
              else
                builtins.trace expected
                  builtins.trace
                  actual
                  false;
          in
          eq (getMessages c.config.assertions) [
            ''
              hercules-ci-agent: Please do not set `trusted-users` in `nix.extraOptions`.
            
              The hercules-ci-agent module by default relies on `nix.settings.trusted-users`
              to be effectful, but a line like `trusted-users = ...` in `nix.extraOptions`
              will override the value set in `nix.settings.trusted-users`.

              Instead of setting `trusted-users` in the `nix.extraOptions` string, you should
              set an option with additive semantics, such as
               - the NixOS option `nix.settings.trusted-users`, or
               - the Nix option in the `extraOptions` string, `extra-trusted-users`
            ''
          ];
        message = "Should have caught a problem with trusted-user";
      }
    ];
  };
  nixpkgs = pkgs.path;
  machine = import nix-darwin {
    inherit nixpkgs system configuration;
  };
in
machine.system
