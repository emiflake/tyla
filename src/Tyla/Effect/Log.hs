{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tyla.Effect.Log
  ( Log (..),
    LogLevel (..),
    debug,
    info,
    warn,
    err,
    runLogStdout,
    LogStdoutC (..),
  )
where

import Control.Algebra
import Control.Carrier.Reader
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import GHC.Generics (Generic1)
import Prelude hiding (log)
import Text.Printf

data LogLevel
  = Debug
  | Info
  | Warn
  | Err
  deriving (Bounded, Eq, Ord)

instance Show LogLevel where
  show logLevel =
    case logLevel of
      Debug -> "[debug]"
      Info -> "[info]"
      Warn -> "[warn]"
      Err -> "[err]"

data Log (m :: * -> *) k
  = Log Text LogLevel (m k)
  deriving stock (Functor, Generic1)
  deriving (HFunctor, Effect)

debug :: (Has Log sig m) => Text -> m ()
debug msg =
  logRaw msg Debug

info :: (Has Log sig m) => Text -> m ()
info msg =
  logRaw msg Info

warn :: (Has Log sig m) => Text -> m ()
warn msg =
  logRaw msg Warn

err :: (Has Log sig m) => Text -> m ()
err msg =
  logRaw msg Err

logRaw :: (Has Log sig m) => Text -> LogLevel -> m ()
logRaw msg lvl =
  send (Log msg lvl (pure ()))

newtype LogStdoutC m a
  = LogStdoutC {unLogC :: ReaderC LogLevel m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Log :+: sig) (LogStdoutC m) where
  alg c =
    case c of
      L (Log msg lvl next) -> do
        minLvl <- LogStdoutC ask
        liftIO . when (lvl >= minLvl) $
          printf "%s: %s" (show lvl) msg
        next
      R next ->
        LogStdoutC (alg (R (handleCoercible next)))

runLogStdout :: LogLevel -> LogStdoutC m a -> m a
runLogStdout level =
  runReader level . unLogC

