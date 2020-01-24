{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Command where

import           Control.Monad

import           Discord
import qualified Discord.Requests         as R
import           Discord.Types

import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as TIO

import           Data.HList

import           Control.Concurrent.Async
import           Data.IORef

import           Booru.Kona

class DiscordSub a where
  handle :: a -> DiscordHandle -> Event -> IO ()
  {-# MINIMAL handle #-}

data Ping = Ping
instance DiscordSub Ping where
  handle _ dis event = case event of
    MessageCreate m ->
      unless (userIsBot (messageAuthor m)) $ do
        prefixed "\\ping" m
        _ <- restCall dis (R.CreateMessage (messageChannel m) "pong!")
        pure ()
    _               -> pure ()

data Kona = Kona
instance DiscordSub Kona where
  handle _ dis event = case event of
    MessageCreate m -> do
      unless (userIsBot (messageAuthor m)) $ do
        prefixed "\\kona" m
        let args = tail . words . Text.unpack $ (messageText m)
        posts <- doGetPosts 0 1 ("order:random" : args)
        case posts of
          Just posts -> do
            let url = _fileUrl (head posts)
            let embed = Embed { embedTitle = Just "Konachan image"
                              , embedType = Nothing
                              , embedDescription = Nothing
                              , embedUrl = Nothing
                              , embedTimestamp = Nothing
                              , embedColor = Just (0xffffff)
                              , embedFields =
                                [ Image (Text.pack url) "" 0 0
                                ] }
            _ <- restCall dis (R.CreateMessageEmbed (messageChannel m) "" embed)
            pure ()
          Nothing -> do
            _ <- restCall dis (R.CreateMessage (messageChannel m) "Sad")
            pure ()

        pure ()
    _               -> pure ()

newtype Counter = Counter (IORef Int)
instance DiscordSub Counter where
  handle (Counter ref) dis event = case event of
    MessageCreate m ->
      unless (userIsBot (messageAuthor m)) $ do
        prefixed "\\count" m
        val <- atomicModifyIORef ref (\v -> (succ v, succ v))
        _ <- restCall dis (R.CreateMessage (messageChannel m) $ "incremented! new value: " <> (Text.pack . show $ val))
        pure ()
    _               -> pure ()

instance All DiscordSub xs => DiscordSub (HList xs) where
  handle HNil _ _          = pure ()
  handle (x :# xs) dis evt = handle x dis evt >> handle xs dis evt

-- UTIL

prefixed :: Text -> Message -> IO ()
prefixed prefix m = guard (prefix `Text.isPrefixOf` messageText m)
