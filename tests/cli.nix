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

  # Test hci effect list with herculesCI structure
  mkdir -p test-repo-list
  cd test-repo-list
  git init
  git config user.email "test@example.com"
  git config user.name "Test User"

  # Create a herculesCI structure with multiple effects
  cat >ci.nix <<EOF
  { src ? builtins.getEnv "PWD" }:
  {
    herculesCI = ciArgs: {
      onPush.default.outputs = {
        effects = {
          deploy = {
            main = derivation {
              name = "deploy-effect";
              system = builtins.currentSystem;
              builder = "${hello}/bin/hello";
              isEffect = true;
            };
            # Effect with attribute name that needs quoting
            "hercules-ci.com" = derivation {
              name = "quoted-attr-effect";
              system = builtins.currentSystem;
              builder = "${hello}/bin/hello";
              isEffect = true;
            };
          };
          notify = derivation {
            name = "notify-effect";
            system = builtins.currentSystem;
            builder = "${hello}/bin/hello";
            isEffect = true;
          };
        };
      };
      onSchedule.nightly.outputs = {
        effects.backup = derivation {
          name = "backup-effect";
          system = builtins.currentSystem;
          builder = "${hello}/bin/hello";
          isEffect = true;
        };
      };
    };
  }
  EOF

  git add ci.nix
  git commit -m "Add herculesCI effects for list test"

  git remote add origin ../remote.git
  git push -u origin master:list-test

  # Test hci effect list
  output=$(hci effect list --no-token --project github/test-owner/test-repo)
  echo "hci effect list output:"
  echo "$output"

  # Check that all effects are listed (order may vary)
  echo "$output" | grep "onPush.default.effects.deploy.main" > /dev/null
  echo "$output" | grep "onPush.default.effects.notify" > /dev/null
  echo "$output" | grep "onSchedule.nightly.effects.backup" > /dev/null
  # Quoted attribute name should be properly formatted (nested under deploy)
  echo "$output" | grep 'onPush.default.effects.deploy."hercules-ci.com"' > /dev/null

  # Test hci effect eval with quoted attribute path
  output=$(hci effect eval --no-token --project github/test-owner/test-repo 'onPush.default.effects.deploy."hercules-ci.com"')
  echo "hci effect eval (quoted attr) output: $output"
  [[ "$output" =~ ^/nix/store/[a-z0-9]+-quoted-attr-effect\.drv$ ]]

  cd ..

  # Test hci effect list with effect outside effects attribute (should only list effects in effects attr)
  mkdir -p test-repo-list-wrong-location
  cd test-repo-list-wrong-location
  git init
  git config user.email "test@example.com"
  git config user.name "Test User"

  cat >ci.nix <<EOF
  { src ? builtins.getEnv "PWD" }:
  {
    herculesCI = ciArgs: {
      onPush.default.outputs = {
        effects.valid = derivation {
          name = "valid-effect";
          system = builtins.currentSystem;
          builder = "${hello}/bin/hello";
          isEffect = true;
        };
        # This effect is NOT in the effects attribute - should not be listed
        wrongLocation = derivation {
          name = "wrong-location-effect";
          system = builtins.currentSystem;
          builder = "${hello}/bin/hello";
          isEffect = true;
        };
        # This throws - should not cause list to fail since we only look in effects
        throwing = throw "This should not be evaluated by hci effect list";
      };
    };
  }
  EOF

  git add ci.nix
  git commit -m "Add herculesCI with misplaced effect"

  git remote add origin ../remote.git
  git push -u origin master:list-wrong-location-test

  output=$(hci effect list --no-token --project github/test-owner/test-repo)
  echo "hci effect list (wrong location) output:"
  echo "$output"

  # Should list the valid effect
  echo "$output" | grep "onPush.default.effects.valid" > /dev/null
  # Should NOT list the wrongly located effect
  if echo "$output" | grep "wrongLocation" > /dev/null; then
    echo "ERROR: Found wrongLocation in output - effects outside 'effects' attr should not be listed"
    exit 1
  fi

  cd ..

  # Test hci effect list with traditional ci.nix (simple format, no herculesCI)
  mkdir -p test-repo-list-simple
  cd test-repo-list-simple
  git init
  git config user.email "test@example.com"
  git config user.name "Test User"

  # Traditional format: effects directly at top level, no onPush/onSchedule
  cat >ci.nix <<EOF
  {
    simpleEffect = derivation {
      name = "simple-list-effect";
      system = builtins.currentSystem;
      builder = "${hello}/bin/hello";
      isEffect = true;
    };
    nested.deepEffect = derivation {
      name = "deep-list-effect";
      system = builtins.currentSystem;
      builder = "${hello}/bin/hello";
      isEffect = true;
    };
  }
  EOF

  git add ci.nix
  git commit -m "Add simple ci.nix for list test"

  git remote add origin ../remote.git
  git push -u origin master:list-simple-test

  output=$(hci effect list --no-token --project github/test-owner/test-repo)
  echo "hci effect list (simple) output:"
  echo "$output"

  echo "$output" | grep -q "simpleEffect"
  echo "$output" | grep -q "nested.deepEffect"

  cd ..

  # Test --pretend-tag option
  mkdir -p test-repo-pretend-tag
  cd test-repo-pretend-tag
  git init
  git config user.email "test@example.com"
  git config user.name "Test User"

  # Create an effect that checks the tag value
  cat >ci.nix <<EOF
  { src ? builtins.getEnv "PWD" }:
  {
    herculesCI = ciArgs:
      # Assert that the tag is passed correctly when using --pretend-tag
      assert ciArgs.primaryRepo.tag == "v1.0.0";
      {
        onPush.default.outputs = {
          effects.taggedEffect = derivation {
            name = "tagged-effect";
            system = builtins.currentSystem;
            builder = "${hello}/bin/hello";
            isEffect = true;
          };
        };
      };
  }
  EOF

  git add ci.nix
  git commit -m "Add effect with tag assertion"

  git remote add origin ../remote.git
  git push -u origin master:pretend-tag-test

  # Test that --pretend-tag sets the tag correctly
  output=$(hci effect eval --no-token --project github/test-owner/test-repo --pretend-tag v1.0.0 onPush.default.effects.taggedEffect)
  echo "hci effect eval (--pretend-tag) output: $output"
  [[ "$output" =~ ^/nix/store/[a-z0-9]+-tagged-effect\.drv$ ]]

  # Test that --as-tag alias also works
  output=$(hci effect eval --no-token --project github/test-owner/test-repo --as-tag v1.0.0 onPush.default.effects.taggedEffect)
  echo "hci effect eval (--as-tag) output: $output"
  [[ "$output" =~ ^/nix/store/[a-z0-9]+-tagged-effect\.drv$ ]]

  cd ..

  touch $out
''
