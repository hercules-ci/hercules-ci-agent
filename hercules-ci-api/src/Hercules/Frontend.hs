{-# LANGUAGE DataKinds #-}

module Hercules.Frontend where

import qualified Data.Text as T
import Hercules.API.Accounts.Account (Account)
import Hercules.API.Name (Name)
import Hercules.API.Prelude
import Hercules.API.Projects.Project (Project)
import Hercules.API.SourceHostingSite.SourceHostingSite
  ( SourceHostingSite,
  )
import Network.URI
import Servant.API
import Servant.API.Generic
import Servant.Links

-- | URL routes for the web interface.
--
-- Typically the base URL for this is https://hercules-ci.com
data FrontendRoutes view f
  = FrontendRoutes
      { home ::
          f
            :- view,
        account ::
          f
            :- Capture' [Required, Strict] "site" (Name SourceHostingSite)
            :> Capture' [Required, Strict] "account" (Name Account)
            :> view,
        project ::
          f
            :- Capture' [Required, Strict] "site" (Name SourceHostingSite)
            :> Capture' [Required, Strict] "account" (Name Account)
            :> Capture' [Required, Strict] "project" (Name Project)
            :> view,
        job ::
          f
            :- Capture' [Required, Strict] "site" (Name SourceHostingSite)
            :> Capture' [Required, Strict] "account" (Name Account)
            :> Capture' [Required, Strict] "project" (Name Project)
            :> "jobs"
            :> Capture' [Required, Strict] "jobIndex" Int
            :> view
      }
  deriving (Generic)

mkLinks :: URI -> FrontendRoutes Raw (AsLink Text)
mkLinks base = allFieldLinks' $
  \link -> shows2Text $ uriToString id $ linkURI link `relativeTo` base
  where
    shows2Text :: ShowS -> Text
    shows2Text = T.pack . ($ "")

herculesLinks :: FrontendRoutes Raw (AsLink Text)
herculesLinks = mkLinks base
  where
    base :: URI
    base = URI
      { uriPath = "",
        uriQuery = "",
        uriFragment = "",
        uriScheme = "https:",
        uriAuthority = Just URIAuth
          { uriUserInfo = "",
            uriRegName = "hercules-ci.com",
            uriPort = ""
          }
      }
