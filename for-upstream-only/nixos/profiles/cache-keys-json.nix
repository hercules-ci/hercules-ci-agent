# TODO (doc) CacheKeys format reference link
/*
  A module that configures an agent machine to use caches for reading and/or
  writing.
 */
{ pkgs, lib, config, ...}:
let
  cfg = config.profile.cacheKeys;
  inherit (lib.lists) filter concatMap concatLists;
  inherit (lib) types isAttrs mkIf escapeShellArg attrValues mapAttrsToList filterAttrs;
  inherit (builtins) readFile fromJSON map split;

  allBinaryCaches = 
    if cfg.file == null
    then {}
    else builtins.fromJSON (builtins.readFile cfg.file);

  cachixCaches =
    filterAttrs (k: v: (v.kind or null) == "CachixCache") allBinaryCaches;

  cachixVersionWarnings =
    mapAttrsToList (k: v: "In file ${cfg.file}, entry ${k}, unsupported CachixCache version ${(v.apiVersion or null)}")
    (filterAttrs (k: v: (v.apiVersion or null) != null) cachixCaches);

  otherCaches =
    filterAttrs (k: v: (v.kind or null) != "CachixCache") allBinaryCaches;
  otherCachesWarnings =
    mapAttrsToList (k: v: "In file ${cfg.file}, entry ${k}, unsupported cache with kind ${(v.kind or null)}") otherCaches;

  pubkeys = concatMap (cache: cache.publicKeys) (attrValues cachixCaches);

in
{
  options.profile.cacheKeys = {
    file = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        A CacheKeys JSON file, to be read during evaluation. This can be a
        path expression, which will not be loaded into the Nix store by the
        declaring module.
      ''; # TODO (doc) CacheKeys format reference link
    };
    deployedPath = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        The path to the deployed profile.cacheKeys.file on the target
        machine(s). This should be a plain string
        literal, to avoid accidentally copying secrets into the Nix store.

        The file will be read by root and any pull credentials will be made
        available exclusively to the Nix daemon. Non-root users of Nix tools
        may need to provide the credentials themselves to function properly.
        For this reason the caches are merely trusted and not enabled by default.
      '';
    };

  };

  config = lib.mkIf ( # || because of the assertions
                     cfg.file != null || cfg.deployedPath != null
                    ) {

    warnings = otherCachesWarnings ++ cachixVersionWarnings;

    assertions = [
      { assertion = cfg.deployedPath != null -> cfg.file != null;
        message = ''
          You need to specify profile.cacheKeys.file in order to provide the
          non-sensitive parts of the cache configuration.

          WARNING: If you've used a path expression in cfg.deployedPath,
          you may want to delete it from your Nix store!
        '';
      }

      # TODO: not required when file is not sensitive.
      # TODO (doc) CacheKeys format reference link
      { assertion = cfg.file != null -> cfg.deployedPath != null;
        message = ''
          You need to deploy the CacheKeys JSON file to the machine outside the
          Nix store and set profile.cacheKeys.deployedPath to the location of the
          deployed file.
        '';
      }
    ];

    nix.trustedBinaryCaches = ["https://cache.nixos.org"] ++ mapAttrsToList (name: keys: "https://${name}.cachix.org") cachixCaches;
    nix.binaryCachePublicKeys = concatLists (mapAttrsToList (name: keys: keys.publicKeys) cachixCaches);

    nix.extraOptions = ''
      netrc-file = /etc/nix/daemon-netrc
    '';

    systemd.paths.cache-keys-json = {
      wantedBy = [ "multi-user.target" ];
      pathConfig.PathExists = cfg.deployedPath;
      pathConfig.PathChanged = cfg.deployedPath;
      pathConfig.Unit = "cache-keys-install.service";
    };

    systemd.services.cache-keys-install = {
      requires = [ "cache-keys-json.path" ];
      serviceConfig.Type = "oneshot";
      script = ''
        ${pkgs.jq}/bin/jq -r <${escapeShellArg cfg.deployedPath} \
            'to_entries[] | .key as $key | .value | select (.kind == "CachixCache") | .authToken | select (. != null) | "machine \($key).cachix.org password \(.)" ' \
          | install --mode=0400 --owner=root --group=root \
              /dev/stdin \
              /etc/nix/daemon-netrc \
        ;
      '';
    };
  };
}
