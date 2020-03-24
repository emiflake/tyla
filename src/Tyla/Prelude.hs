module Tyla.Prelude
  ( module Tyla.Reactor,
    showText,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Tyla.Reactor

showText :: Show a => a -> Text
showText = Text.pack . show
