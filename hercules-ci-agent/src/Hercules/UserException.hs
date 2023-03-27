module Hercules.UserException where

import Control.Exception (Exception (displayException))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text qualified as T
import Prelude (Show (..))

-- | 'Exception' representation of errors that may be caused by user error.
data UserException = UserException Text

instance Exception UserException where
  displayException = show

instance Show UserException where
  show (UserException msg) = "error: " <> T.unpack msg
