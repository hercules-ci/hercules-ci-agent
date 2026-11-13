module Spec where

import Hercules.CLI.CredentialsSpec
import Hercules.CLI.CurlSpec
import Hercules.CLI.JSONSpec
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "The CLI" $ do
  Hercules.CLI.CredentialsSpec.spec
  Hercules.CLI.CurlSpec.spec
  Hercules.CLI.JSONSpec.spec
