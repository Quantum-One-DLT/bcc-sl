module Pos.Core.Metrics.Constants (
      bccNamespace
    , withBccNamespace
                                        ) where

import           Universum

bccNamespace :: Text
bccNamespace = "bcc"

withBccNamespace :: Text -> Text
withBccNamespace label = bccNamespace <> "." <> label
