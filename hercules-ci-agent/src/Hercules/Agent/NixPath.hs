module Hercules.Agent.NixPath
  ( renderNixPath,
    renderNixPathElement,
    renderSubPath,
  )
where

import Data.Text qualified as T
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Protolude

renderNixPath ::
  [EvaluateTask.NixPathElement (EvaluateTask.SubPathOf FilePath)] ->
  Text
renderNixPath = T.intercalate ":" . map renderNixPathElement

renderNixPathElement ::
  EvaluateTask.NixPathElement
    (EvaluateTask.SubPathOf FilePath) ->
  Text
renderNixPathElement pe =
  foldMap (<> "=") (EvaluateTask.prefix pe)
    <> renderSubPath (toS <$> EvaluateTask.value pe)

renderSubPath :: EvaluateTask.SubPathOf Text -> Text
renderSubPath sp =
  toS (EvaluateTask.path sp) <> foldMap ("/" <>) (EvaluateTask.subPath sp)
