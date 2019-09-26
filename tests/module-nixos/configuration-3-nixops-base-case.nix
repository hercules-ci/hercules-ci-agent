{ ... }: {

  imports = [ ./configuration-2-base-case.nix ];

  deployment.keys."binary-caches.json".text = "{}";
  deployment.keys."cluster-join-token.key".keyFile = ./cluster-join-token.key;

}
