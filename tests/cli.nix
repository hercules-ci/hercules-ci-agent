{ hci, jq, nodePackages, runCommand }:

runCommand "test-cli"
{
  nativeBuildInputs = [ hci jq nodePackages.json-diff ];
} ''
  >expected cat <<EOF
  {
    "kind": "Secret",
    "data": {
      "foo": "bar"
    },
    "condition": {
      "and": [
        {
          "isOwner": "hercules-ci"
        },
        {
          "isRepo": "dummy"
        },
        "isDefaultBranch"
      ]
    }
  }
  EOF
  >actual hci secret echo --project github/hercules-ci/dummy --string foo bar
  json-diff expected actual
  touch $out
''
