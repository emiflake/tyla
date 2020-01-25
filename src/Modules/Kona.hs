{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Modules.Kona
  ( kona
  ) where

import           Booru.Kona
import           Data.List
import qualified Data.Text             as T

import           Control.Monad

import qualified Discord.Requests      as R
import           Discord.Types

import           Control.Effect
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Effect.Discord

urlEmbed :: T.Text -> T.Text -> Embed
urlEmbed title url =
  Embed { embedTitle = Just title
        , embedType = Nothing
        , embedDescription = Nothing
        , embedUrl = Nothing
        , embedTimestamp = Nothing
        , embedColor = Just 0xffffff
        , embedFields = [ Image url "" 0 0]
        }

kona :: ( Carrier sig m
        , Member Discord sig
        , Member (Lift IO) sig
        , Member (Reader Event) sig
        ) => m ()
kona =
  ask @Event >>= \case
    (MessageCreate m) ->
      when (not (userIsBot (messageAuthor m)) && "\\kona" `T.isPrefixOf` messageText m) $ do
        let args = tail . words . T.unpack $ messageText m
        posts <- sendM @IO $ doGetPosts 0 1 ("order:random" : args)
        case posts of
          Just (x:_) -> do
            let url = _fileUrl x
            void $ disCall (R.CreateMessageEmbed (messageChannel m) "" (urlEmbed "Konachan image" (T.pack url)))
          _ ->
            void $ disCall (R.CreateMessage (messageChannel m) "Sad")
    _ -> pure ()
