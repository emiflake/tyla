{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Command where

import           Control.Monad
import           Control.Monad.IO.Class

import           Discord                  hiding (disCall)
import qualified Discord.Requests         as R
import           Discord.Types

import           Data.Text                (Text)
import qualified Data.Text                as Text

import           Data.Foldable
import           Data.HList

import           Control.Concurrent.Async
import           Data.IORef

import           Booru.Kona

import           Control.Effect
import           Control.Effect.Carrier   hiding (handle)
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Effect.Discord

import           Data.Proxy

-- class DiscordSub a where
--   type family M a :: * -> *
--   handle :: ( Member (Lift IO) sig
--             , Member (Reader a) sig
--             , Member Discord sig
--             , Carrier sig m
--             ) => Proxy a -> Event -> (M a) ()
--   {-# MINIMAL handle #-}

-- data Ping = Ping
-- instance DiscordSub Ping where
--   handle proxy event = case event of
--     MessageCreate m -> do
--         _ <- disCall (R.CreateMessage (messageChannel m) "pong!")
--         pure ()
--     _               -> pure ()

-- data Kona = Kona
-- instance DiscordSub Kona where
--   handle proxy event = case event of
--     MessageCreate m -> do
--         let args = tail . words . Text.unpack $ messageText m
--         posts <- liftIO $ doGetPosts 0 1 ("order:random" : args)
--         case posts of
--           Just posts' -> do
--             let url = _fileUrl (head posts')
--             let embed = Embed { embedTitle = Just "Konachan image"
--                               , embedType = Nothing
--                               , embedDescription = Nothing
--                               , embedUrl = Nothing
--                               , embedTimestamp = Nothing
--                               , embedColor = Just 0xffffff
--                               , embedFields =
--                                 [ Image (Text.pack url) "" 0 0
--                                 ] }
--             void $ disCall (R.CreateMessageEmbed (messageChannel m) "" embed)
--           Nothing ->
--             void $ disCall (R.CreateMessage (messageChannel m) "Sad")
--     _               -> pure ()

-- newtype Counter = Counter (IORef Int)
-- instance DiscordSub Counter where
--   handle proxy event = case event of
--     MessageCreate m -> do
--         (Counter ref) <- ask @Counter
--         val <- liftIO $ atomicModifyIORef ref (\v -> (succ v, succ v))
--         _ <- disCall (R.CreateMessage (messageChannel m) $ "incremented! new value: " <> (Text.pack . show $ val))
--         pure ()
--     _               -> pure ()

-- instance All DiscordSub xs => DiscordSub (HList xs) where
--   handle proxy evt = do
--     subs <- ask @(HList xs)
--     mapM async (accum subs) >>= traverse_ wait
--     where
--       accum :: All DiscordSub xs => HList xs -> [m ()]
--       accum HNil      = []
--       accum (x :# xs) = handle proxy evt : accum xs
