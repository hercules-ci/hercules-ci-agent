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
    }
  }
  EOF
  >actual hci secret echo --string foo bar
  json-diff expected actual
  touch $out
''
