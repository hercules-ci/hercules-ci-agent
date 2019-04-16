Example releasing hercules-ci-agent, although same could be done for hercules-ci-api:

- git checkout -B v0.X.X # or PVP for api package
- bump version in hercules-ci-agent/hercules-ci-agent.cabal
- using git log update hercules-ci-agent/CHANGELOG.md
- git commit -m "hercules-ci-agent-0.X.X" # or PVP for api package
- copy the contents of the changelog for this release to: git tag -s -a hercules-ci-agent-0.X.X
- git push --tags
- open a PR against master
- open a PR against stable using the tag as a base
