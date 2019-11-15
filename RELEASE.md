Example releasing hercules-ci-agent and optionally hercules-ci-api:

### 1. Prepare API

- git checkout -B vX.X.X (new agent version)
- update hercules-ci-api/CHANGELOG.md
   - git log PREVIOUS..HEAD -- ./hercules-ci-api
- bump api version
   - hercules-ci-api/CHANGELOG.md
   - hercules-ci-api/hercules-ci-api.cabal
   - hercules-ci-agent/hercules-ci-agent.cabal
- scripts/generate-nix
- git add --patch
- git commit -m "hercules-ci-api-Y.Y.Y.Y"

### 2. Prepare Agent

- update hercules-ci-agent/CHANGELOG.md
   - git log PREVIOUS..HEAD -- ./for-upstream ./hercules-ci-agent
- bump agent version
   - hercules-ci-agent/CHANGELOG.md
   - hercules-ci-agent/hercules-ci-agent.cabal
   - for-upstream/common.nix package option
- add a markdown link in CHANGELOG.md markdown link for the added version tag (header and link map at the bottom)
- scripts/generate-nix
- pre-commit run -a
- git add --patch
- git commit -m "hercules-ci-agent-X.X.X"
- wait for CI to succeed
- open a PR against master

### 3. Preliminary tag and verification

- git log -2
- copy the contents of the api changelog for this release to: git tag -s -a hercules-ci-api-Y.Y.Y.Y API_COMMIT
- copy the contents of the agent changelog for this release to: git tag -s -a hercules-ci-agent-X.X.X AGENT_COMMIT
- git push --tags
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

### 4. Finalize the release

- merge hercules-ci-agent release into master
- open a PR against stable using the release branch as a base
- open a PR on nix-darwin (from previous step's branch)
- make sure to verify default package url is accessible
   - update agent-deployments to stable
