{ pkgs, lib, config, ...}:
let
  cfg = config.services.hercules-ci-agent;
  inherit (cfg) finalConfig;
  inherit (lib) mkIf escapeShellArg;

in
{
  config = mkIf (cfg.enable && (cfg.finalConfig.binaryCachesPath or null) != null) {

    systemd.paths.cache-keys-json = {
      wantedBy = [ "multi-user.target" ];
      pathConfig.PathExists = cfg.finalConfig.binaryCachesPath;
      pathConfig.PathChanged = cfg.finalConfig.binaryCachesPath;
      pathConfig.Unit = "cache-keys-install.service";
    };

    systemd.services.cache-keys-install = {
      requires = [ "cache-keys-json.path" ];
      before = [ "hercules-ci-agent.service" ];
      serviceConfig.Type = "oneshot";
      script = ''
        ${pkgs.jq}/bin/jq -r <${escapeShellArg cfg.finalConfig.binaryCachesPath} \
            'to_entries[] | .key as $key | .value | select (.kind == "CachixCache") | .authToken | select (. != null) | "machine \($key).cachix.org password \(.)" ' \
          | install --mode=0400 --owner=root --group=root \
              /dev/stdin \
              /etc/nix/daemon-netrc \
        ;
      '';
    };

  };
}
