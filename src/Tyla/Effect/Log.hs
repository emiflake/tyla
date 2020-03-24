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
    logDebug,
    logInfo,
    logWarning,
    logError,
    Logger,
    newLogger,
    runLogStdout,
    LogStdoutC (..),
    log,
  )
where

import Control.Algebra
import Control.Carrier.Reader
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import GHC.Generics (Generic1)
import System.Log.FastLogger (TimedFastLogger, ToLogStr, toLogStr)
import qualified System.Log.FastLogger as FastLogger
import Prelude hiding (log)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Bounded, Eq, Ord)

instance ToLogStr LogLevel where
  toLogStr logLevel =
    case logLevel of
      Debug -> "[debug]"
      Info -> "[info]"
      Warning -> "[warning]"
      Error -> "[error]"

instance Show LogLevel where
  show logLevel =
    case logLevel of
      Debug -> "debug"
      Info -> "info"
      Warning -> "warning"
      Error -> "error"

data Log (m :: * -> *) k
  = Log Text LogLevel (m k)
  deriving stock (Functor, Generic1)
  deriving (HFunctor, Effect)

logDebug :: (Has Log sig m) => Text -> m ()
logDebug msg =
  logRaw msg Debug

logInfo :: (Has Log sig m) => Text -> m ()
logInfo msg =
  logRaw msg Info

logWarning :: (Has Log sig m) => Text -> m ()
logWarning msg =
  logRaw msg Warning

logError :: (Has Log sig m) => Text -> m ()
logError msg =
  logRaw msg Error

logRaw :: (Has Log sig m) => Text -> LogLevel -> m ()
logRaw msg lvl =
  send (Log msg lvl (pure ()))

newtype LogStdoutC m a
  = LogStdoutC {unLogC :: ReaderC (Logger, LogLevel) m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Log :+: sig) (LogStdoutC m) where
  alg c =
    case c of
      L (Log msg lvl next) -> do
        (logger, minLvl) :: (Logger, LogLevel) <- LogStdoutC ask
        liftIO $ when (lvl >= minLvl) (log logger lvl msg)
        next
      R next ->
        LogStdoutC (alg (R (handleCoercible next)))

runLogStdout :: Logger -> LogLevel -> LogStdoutC m a -> m a
runLogStdout logger level =
  runReader (logger, level) . unLogC

type Logger =
  TimedFastLogger

newLogger :: IO Logger
newLogger = do
  timeCache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  (logger, _cleanUp) <-
    FastLogger.newTimedFastLogger
      timeCache
      (FastLogger.LogStdout FastLogger.defaultBufSize)
  pure logger

log :: FastLogger.ToLogStr a => Logger -> LogLevel -> a -> IO ()
log logger lvl msg =
  logger (\t -> toLogStr t <> " " <> toLogStr lvl <> " " <> toLogStr msg <> "\n")
