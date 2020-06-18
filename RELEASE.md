Example releasing hercules-ci-agent and optionally hercules-ci-api:

### Release

    git checkout -B release
    ./scripts/releaser

Base the tag message on the changelog, but removing the `###` prefix from headers (avoid accidental comments).

### Dependents

- update nix-darwin
   - prepare branch on nix-darwin clone
      - git fetch upstream
      - git checkout hercules-ci-agent -b hercules-ci-agent-releasing-X.X.X
      - git merge --no-ff upstream/master
   - for-upstream/copy-to-nix-darwin <path-to-nix-darwin-clone>
      - check it
   - commit -m hercules-ci-agent: x.x.x -> X.X.X
   - push
- update agent-deployments
   - niv update nix-darwin -b hercules-ci-agent-releasing-X.X.X
   - niv update nixpkgs      # -b nixos-P.Q in case of NixOS update
- deploy agent-deployments (staging, PR, merge, prod)
- test
   - push dummy change in hercules-ci-agent/hercules-ci-agent/Main.hs to ci-trigger for testing
   - check https://hercules-ci.com/github/hercules-ci/hercules-ci-agent/

- update terraform-hercules-ci
   - niv update hercules-ci-agent -b hercules-ci-agent-<VERSION>
   - ./update
- update example-deploy-agent-terraform-aws
   - (after merge of terraform-hercules-ci master)
   - ./update

### Finalize

- merge the release branch
- make sure all tags are pushed
   - hercules-ci-agent
   - terraform-hercules-ci
   - example-deploy-agent-terraform-aws
