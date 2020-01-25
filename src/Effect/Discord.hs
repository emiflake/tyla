{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Effect.Discord
  ( Discord(..)
  , DiscordC(..)
  , disCall
  , Effect.Discord.runDiscord
  ) where

import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Monad.IO.Class        (MonadIO, liftIO)

import           Discord
import           Discord.Internal.Rest.Prelude

data Discord (m :: * -> *) k where
  RestCall :: (FromJSON a, Request (r a)) => r a -> (Either RestCallErrorCode a -> m k) -> Discord m k

deriving instance Functor m => Functor (Discord m)

instance HFunctor Discord where
  hmap f (RestCall req after) =
      RestCall req (f . after)

instance Effect Discord where
  handle state handler (RestCall req after) =
    RestCall req (handler . (<$ state) . after)

disCall :: ( Member Discord sig
           , Carrier sig m
           , FromJSON a
           , Request (r a)
           ) => r a
        -> m (Either RestCallErrorCode a)
disCall req =
  send (RestCall req pure)

newtype DiscordC m a =
  DiscordC { unDiscordC :: ReaderC DiscordHandle m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Carrier sig m, MonadIO m) => Carrier (Discord :+: sig) (DiscordC m) where
  eff c =
    case c of
      L (RestCall req cb) -> do
        dis <- DiscordC ask
        resp <- liftIO $ restCall dis req
        cb resp
      R next ->
        DiscordC (eff (R (handleCoercible next)))

runDiscord :: DiscordHandle -> DiscordC m a -> m a
runDiscord dis =
  runReader dis . unDiscordC
