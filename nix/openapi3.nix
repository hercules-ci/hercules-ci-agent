{ jq, hercules-ci-api, runCommand, ... }:

runCommand "hercules-openapi3"
{
  LANG = "C.UTF-8";
  nativeBuildInputs = [ jq hercules-ci-api ];
} ''
  mkdir $out
  api=$out/hercules-ci-api-openapi3.json
  hercules-gen-swagger --experimental-openapi3 >$api
  echo operations:
  jq '.paths.[].[].operationId' < $api | sort
  echo checking against braces
  (jq '.paths.[].[].operationId' < $api) | (
    if grep -F '{'; then
      echo "found braces"
      echo "add more rules to hercules-ci-api/hercules-gen-swagger/Main.hs"
      exit 1
    else
      echo "all good"
    fi
  )
''
