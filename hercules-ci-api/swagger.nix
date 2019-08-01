{ hercules-ci-api, runCommand, ... }:

runCommand "hercules-swagger" {
  LANG = "C.UTF-8";
} ''
  mkdir $out
  ${hercules-ci-api}/bin/hercules-gen-swagger >$out/swagger.json
''
