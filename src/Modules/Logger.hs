{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Modules.Logger
  ( loggerModule
  ) where

import qualified Data.Text             as T

import           Discord.Types

import           Control.Effect
import           Control.Effect.Reader
import           Effect.Log

loggerModule :: ( Member Log sig
                , Member (Reader Event) sig
                , Carrier sig m
                ) => m ()
loggerModule = ask @Event >>= \case
  (MessageCreate m) ->
    logDebug $ T.pack (show (userId $ messageAuthor m))
            <> "~"
            <> T.pack (show (userName $ messageAuthor m))
            <> ": "
            <> T.pack (show (messageText m))
  _ -> pure ()
