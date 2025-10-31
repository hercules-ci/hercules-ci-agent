{ hci, jq, nodePackages, runCommand, hello, git }:

runCommand "test-cli"
{
  nativeBuildInputs = [ hci jq nodePackages.json-diff git ];
} ''
  set -x
  hci --version | grep -E 'hci [0-9.]+'
  hci --version | grep -F 'hci ${hci.version}'
  [[ "$(hci --version)" == "$(hci version)" ]]

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

  # Test hci effect eval
  export HOME=$PWD/home
  mkdir -p $HOME

  # Create a bare git repo to use as remote
  git init --bare remote.git

  mkdir -p test-repo
  cd test-repo
  git init
  git config user.email "test@example.com"
  git config user.name "Test User"

  # Create a simple effect directly (bypass herculesCI structure for simplicity)
  cat >ci.nix <<EOF
  {
    simpleEffect = derivation {
      name = "simple-effect";
      system = builtins.currentSystem;
      builder = "${hello}/bin/hello";
      isEffect = true;
    };
  }
  EOF

  git add ci.nix
  git commit -m "Add effect"

  # Set up remote and push
  git remote add origin ../remote.git
  git push -u origin master

  # Test that hci effect eval prints a derivation path
  output=$(hci effect eval --no-token simpleEffect --project github/foo-owner/bar-repo)
  echo "hci effect eval output: $output"

  # Check that output is a store path ending in .drv
  [[ "$output" =~ ^/nix/store/[a-z0-9]+-simple-effect\.drv$ ]]

  cd ..

  # Test hci effect eval with herculesCI structure
  mkdir -p test-repo-herculesci
  cd test-repo-herculesci
  git init
  git config user.email "test@example.com"
  git config user.name "Test User"

  # Create an effect with the full herculesCI structure
  cat >ci.nix <<EOF
  { src ? builtins.getEnv "PWD" }:
  {
    herculesCI = ciArgs:
      # Assert that the project owner and repo are passed correctly from --project flag
      assert ciArgs.primaryRepo.owner == "my-org";
      assert ciArgs.primaryRepo.name == "my-repo";
      {
        onPush.default.outputs = {
          effects.myEffect = derivation {
            name = "herculesci-effect";
            system = builtins.currentSystem;
            builder = "${hello}/bin/hello";
            isEffect = true;
          };
        };
      };
  }
  EOF

  git add ci.nix
  git commit -m "Add herculesCI effect"

  # Set up remote and push (use different branch to avoid conflict with first test)
  git remote add origin ../remote.git
  git push -u origin master:herculesci-test

  # Test that hci effect eval can navigate the herculesCI structure
  # The attribute path includes the job name (onPush.default) followed by the path inside outputs
  # Use --no-token and --project to avoid API calls
  output=$(hci effect eval --no-token --project github/my-org/my-repo onPush.default.effects.myEffect)
  echo "hci effect eval (herculesCI) output: $output"

  # Check that output is a store path ending in .drv
  [[ "$output" =~ ^/nix/store/[a-z0-9]+-herculesci-effect\.drv$ ]]

  cd ..

  touch $out
''
