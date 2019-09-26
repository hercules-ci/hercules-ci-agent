{ ... }: {

  imports = [ ./configuration-2-base-case.nix ];

  services.hercules-ci-agent.concurrentTasks = 4; # Number of jobs to run
  services.hercules-ci-agent.binaryCachesFile = ./binary-caches.json;

}
