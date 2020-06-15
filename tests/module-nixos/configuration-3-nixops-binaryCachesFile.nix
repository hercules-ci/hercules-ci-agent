{ ... }: {

  imports = [ ./configuration-2-base-case.nix ];

  services.hercules-ci-agent.concurrentTasks = 4; # Number of jobs to run
  deployment.keys."cluster-join-token.key".keyFile = ./cluster-join-token.key;

  # This is a test case for a migration. Please use this instead:
  #
  #   deployment.keys."binary-caches.json".keyFile = ./binary-caches.json;
  #
  # DON'T DO THIS:
  services.hercules-ci-agent.binaryCachesFile = ./binary-caches.json;

}
