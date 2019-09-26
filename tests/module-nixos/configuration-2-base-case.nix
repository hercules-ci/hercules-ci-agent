{ ... }: {

  imports = [ ./configuration-1-just-import.nix ];

  services.hercules-ci-agent.enable = true;

}
