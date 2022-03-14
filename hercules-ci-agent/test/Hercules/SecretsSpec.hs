{-# LANGUAGE OverloadedStrings #-}

module Hercules.SecretsSpec where

import Hercules.Formats.Secret (Condition (..))
import Hercules.Secrets (SecretContext (..), evalCondition, evalConditionTrace)
import Protolude
import Test.Hspec (Spec, describe, it, shouldBe)

condition :: Condition
condition =
  Or
    [ And
        [ IsOwner "unicorn",
          IsRepo "killer-app",
          -- normally, you'd specify one of these two:
          IsDefaultBranch,
          IsBranch "main"
        ],
      And
        [ IsOwner "unicorn",
          IsRepo "meta",
          IsTag
        ]
    ]

context1 :: SecretContext
context1 =
  SecretContext
    { ownerName = "unicorn",
      repoName = "killer-app",
      isDefaultBranch = True,
      ref = "refs/heads/main"
    }

context2 :: SecretContext
context2 =
  SecretContext
    { ownerName = "unicorn",
      repoName = "meta",
      isDefaultBranch = False,
      ref = "refs/tags/v4.2"
    }

spec :: Spec
spec = do
  describe "evalCondition" $ do
    it "accepts situation 1" $ do
      evalCondition
        SecretContext
          { ownerName = "unicorn",
            repoName = "killer-app",
            isDefaultBranch = True,
            ref = "refs/heads/main"
          }
        condition
        `shouldBe` True

    it "accepts situation 2" $ do
      evalCondition
        context2
        condition
        `shouldBe` True

    it "rejects non-matching owner, with trace" $ do
      evalConditionTrace
        SecretContext
          { ownerName = "mallory",
            repoName = "killer-app",
            isDefaultBranch = True,
            ref = "refs/heads/main"
          }
        condition
        `shouldBe` ( [ "or: Entering",
                       "and: Entering",
                       "isOwner: owner \"mallory\" is not the desired \"unicorn\"",
                       "and: Leaving (false)",
                       "or: Backtracking",
                       "and: Entering",
                       "isOwner: owner \"mallory\" is not the desired \"unicorn\"",
                       "and: Leaving (false)",
                       "or: Leaving (false)"
                     ],
                     False
                   )

    it "rejects non-matching repo" $ do
      evalCondition
        context1 {repoName = "dotfiles"}
        condition
        `shouldBe` False

    it "rejects non-default branch" $ do
      evalCondition
        context1 {isDefaultBranch = False}
        condition
        `shouldBe` False

    it "rejects branch" $ do
      evalCondition
        context1 {ref = "refs/heads/feat-foo"}
        condition
        `shouldBe` False

    it "rejects non-tag" $ do
      evalCondition
        context2 {ref = "refs/heads/main"}
        condition
        `shouldBe` False

    it "rejects mix and match" $ do
      evalCondition
        context1 {repoName = repoName context2}
        condition
        `shouldBe` False
