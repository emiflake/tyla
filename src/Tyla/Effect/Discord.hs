{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tyla.Effect.Discord
  ( Discord (..),
    DiscordC (..),
    disCall,
    Tyla.Effect.Discord.runDiscord,
    sendMessage,
  )
where

import Control.Algebra
import Control.Carrier.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import Data.Text (Text)
import Discord
import Discord.Internal.Rest.Channel
import Discord.Internal.Rest.Prelude
import Discord.Types as Discord

data Discord (m :: * -> *) k where
  RestCall ::
    (FromJSON a, Request (r a)) =>
    r a ->
    (Either RestCallErrorCode a -> m k) ->
    Discord m k

deriving instance Functor m => Functor (Discord m)

instance HFunctor Discord where
  hmap f (RestCall req after) =
    RestCall req (f . after)

instance Effect Discord where
  thread state handler (RestCall req after) =
    RestCall req (handler . (<$ state) . after)

disCall ::
  ( Has Discord sig m,
    FromJSON a,
    Request (r a)
  ) =>
  r a ->
  m (Either RestCallErrorCode a)
disCall req =
  send (RestCall req pure)

sendMessage :: Has Discord sig m => Discord.ChannelId -> Text -> m (Either RestCallErrorCode Discord.Message)
sendMessage channelId message =
  disCall (CreateMessage channelId message)

newtype DiscordC m a
  = DiscordC {unDiscordC :: ReaderC DiscordHandle m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Discord :+: sig) (DiscordC m) where
  alg c =
    case c of
      L (RestCall req cb) -> do
        dis <- DiscordC ask
        resp <- liftIO $ restCall dis req
        cb resp
      R next ->
        DiscordC (alg (R (handleCoercible next)))

runDiscord :: DiscordHandle -> DiscordC m a -> m a
runDiscord dis =
  runReader dis . unDiscordC
