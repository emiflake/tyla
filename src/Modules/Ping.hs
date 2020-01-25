{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Modules.Ping
  ( ping
  ) where

import           Control.Monad

import qualified Discord.Requests      as R
import           Discord.Types

import           Control.Effect
import           Control.Effect.Reader
import           Effect.Discord

ping :: ( Carrier sig m
        , Member Discord sig
        , Member (Reader Event) sig
        ) => m ()
ping =
  ask @Event >>= \case
    (MessageCreate m) ->
        when (not (userIsBot (messageAuthor m)) && messageText m == "\\ping") $
          void $ disCall (R.CreateMessage (messageChannel m) "pong!")
    _ -> pure ()
