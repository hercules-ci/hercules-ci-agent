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

  >expected cat <<EOF
  {
    "kind": "Secret",
    "data": {
      "foo": "bar",
      "wibble": "wobble"
    }
  }
  EOF
  echo wobble | >actual hci secret echo --string foo bar --password wibble
  json-diff expected actual
  touch $out
''
