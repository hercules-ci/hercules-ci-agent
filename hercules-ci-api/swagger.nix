{ hercules-ci-api, runCommand, ... }:

runCommand "hercules-swagger" {
} ''
  mkdir $out
  ${hercules-ci-api}/bin/hercules-gen-swagger >$out/swagger.json
''
