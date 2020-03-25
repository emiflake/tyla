{-# LANGUAGE OverloadedStrings #-}

module Data
  ( User (..),
    getUserStatement,
    getUsersStatement,
  )
where

import Data.Functor.Contravariant
import qualified Data.Text as Text
import qualified Discord as Discord
import qualified Discord.Types as Discord
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement (..))
import Tyla.Prelude as Tyla

-- USER

data User
  = User
      { userId :: Discord.UserId,
        userBalance :: Int
      }
  deriving (Show, Eq)

-- USER STATEMENTS

getUserStatement :: Statement Discord.UserId (Maybe User)
getUserStatement =
  let sql = "SELECT id, balance FROM users WHERE id = $1"
      encoder =
        Encode.param (Encode.nonNullable userIdEncodeValue)
      decoder =
        Decode.rowMaybe userRow
   in Statement sql encoder decoder True

getUsersStatement :: Statement () [User]
getUsersStatement =
  let sql = "SELECT id, balance FROM users"
      decoder =
        Decode.rowList userRow
   in Statement sql (Encode.noParams) decoder True

-- HELPERS

userIdDecodeValue :: Decode.Value Discord.UserId
userIdDecodeValue =
  (Discord.Snowflake . read . Text.unpack) <$> Decode.text

userIdEncodeValue :: Encode.Value Discord.UserId
userIdEncodeValue =
  (\(Discord.Snowflake w) -> showText w) >$< Encode.text

userRow :: Decode.Row User
userRow =
  User
    <$> (Decode.column . Decode.nonNullable) userIdDecodeValue
    <*> (fromIntegral <$> (Decode.column . Decode.nonNullable) Decode.int4)
