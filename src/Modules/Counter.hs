{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Modules.Counter
  ( counter
  ) where

import           Control.Monad

import qualified Data.Text             as T

import qualified Discord.Requests      as R
import           Discord.Types

import           Control.Effect
import           Control.Effect.Reader
import           Effect.Discord
import           Effect.Serial         (Serial)
import qualified Effect.Serial         as Serial

import           Data.Maybe

counter :: ( Carrier sig m
        , Member Discord sig
        , Member (Reader Event) sig
        , Member (Serial "counter" Int) sig
        ) => m ()
counter =
  ask @Event >>= \case
    (MessageCreate m) ->
        when (not (userIsBot (messageAuthor m)) && messageText m == "\\count") $ do
          val <- fromMaybe (0 :: Int) <$> Serial.load @"counter"
          void $ disCall (R.CreateMessage (messageChannel m) $ "counter: " <> T.pack (show val))
          Serial.store @"counter" (val + 1)
    _ -> pure ()
