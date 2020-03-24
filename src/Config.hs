{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config (..),
    read,
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (read)

data Config
  = Config
      { configToken :: Text
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

read :: FilePath -> IO (Maybe Config)
read fp =
  decodeFileStrict fp
