/*
  A module that configures cachix caches for reading, similar in functionality
  to the `cachix use` command. It relies on JSON Lines files produced by cachix.
 */
{ pkgs, lib, config, ...}:
let
  cfg = config.nix.cachix;
  inherit (lib.lists) filter;
  inherit (lib) types isAttrs mkIf escapeShellArg;
  inherit (builtins) fromJSON map split;

  readJSONLines = fileOrNull:
    if fileOrNull == null then [] else
    let
      lines = split "\n" (builtins.readFile fileOrNull);
    in
      map fromJSON (filter (ln: ln != "" && ln != []) lines);

  jsonLines = readJSONLines cfg.secretsFile;

  pubkeys = filter (json: isAttrs json && json.kind == "CachixPublicKey") jsonLines;

in
{
  options.nix.cachix = {
    secretsFile = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        A JSON Lines file produced by the cachix export command. It
        will be read during evaluation. This can be a path expression, which
        will not be loaded into the Nix store by the declaring module.
      '';
    };
    deployedSecretsPath = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        The path to the deployed nix.cachix.secretsFile on the target
        machine(s). This should be a plain string
        literal, to avoid accidentally copying secrets into the Nix store.

        The file will be read by root and any pull credentials will be made
        available exclusively to the Nix daemon. Non-root users of Nix tools
        may need to provide the credentials themselves to function properly.
        For this reason the caches are merely trusted and not enabled by default.
      '';
    };
    /*
      TODO: make this actually configurable
            This module is currently limited to the hardened version which
            deploys netrc to daemon-netrc with 0400, root:root

    rootOnly = lib.mkOption {
      type = types.bool;
      default = true;
      description = ''
        Writes the pull tokens (if any) to a file that is only readable by root
        and the Nix daemon. This makes it harder for users to retrieve these
        tokens.
      '';
    };
    */

  };

  config = lib.mkIf ( # || because of the assertions
                     cfg.secretsFile != null || cfg.deployedSecretsPath != null
                    ) {

    assertions = [
      { assertion = cfg.deployedSecretsPath != null -> cfg.secretsFile != null;
        message = ''
          You need to specify nix.cachix.secretsFile in order to provide the
          non-sensitive parts of the cache configuration.

          WARNING: If you've used a path expression in cfg.deployedSecretsPath,
          you may want to delete it from your Nix store!
        '';
      }

      # TODO: not required when file is not sensitive.
      { assertion = cfg.secretsFile != null -> cfg.deployedSecretsPath != null;
        message = ''
          You need to deploy the Cachix secrets file to the machine outside the
          Nix store and set nix.cachix.deployedSecretsPath to the location of the
          deployed file.
        '';
      }
    ];

    nix.trustedBinaryCaches = ["https://cache.nixos.org"] ++ map (o: "https://${o.cacheName}.cachix.org") pubkeys;
    nix.binaryCachePublicKeys = map (o: o.publicKey) pubkeys;

    nix.extraOptions = ''
      netrc-file = /etc/nix/daemon-netrc
    '';

    systemd.paths.cachix-secrets = {
      wantedBy = [ "multi-user.target" ];
      pathConfig.PathExists = cfg.deployedSecretsPath;
      pathConfig.PathChanged = cfg.deployedSecretsPath;
      pathConfig.Unit = "cachix-install-netrc.service";
    };

    systemd.services.cachix-install-netrc = {
      wantedBy = [ "multi-user.target" ];
      before = [ "nix-daemon.service" ];
      serviceConfig.Type = "oneshot";
      script = ''
        ${pkgs.jq}/bin/jq -r <${escapeShellArg cfg.deployedSecretsPath} \
            'select(.kind == "CachixPullToken") | "machine \(.cacheName).cachix.org password \(.secretToken)"' \
          | install --mode=0400 --owner=root --group=root \
              /dev/stdin \
              /etc/nix/daemon-netrc \
        ;
      '';
    };
  };
}
