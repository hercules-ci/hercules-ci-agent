module Spec where

import Hercules.CLI.CredentialsSpec
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "The CLI" $ do
  Hercules.CLI.CredentialsSpec.spec
