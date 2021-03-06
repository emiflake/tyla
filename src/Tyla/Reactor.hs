{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tyla.Reactor
  ( Reactor (..),
    Tyla.Reactor.run,
    Stack,
  )
where

import Control.Algebra
import Control.Carrier.Lift
import Control.Exception
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Discord
import qualified Discord.Types as Discord
import qualified Hasql.Pool
import Tyla.Effect.Database
import Tyla.Effect.Discord
import Tyla.Effect.Log

type Stack sig m =
  ( Has Log sig m,
    Has Discord sig m,
    Has Database sig m
  )

data Reactor msg
  = Reactor
      { parseEvent :: Discord.Event -> msg,
        handleMsg :: forall sig m. Stack sig m => msg -> m ()
      }

run :: Text -> Reactor msg -> IO ()
run token reactor = do
  pool <- Hasql.Pool.acquire (3, 10, "host=localhost port=5432 dbname=tyla")
  userFacingError <-
    Discord.runDiscord $
      Discord.def
        { Discord.discordToken = token,
          Discord.discordOnEvent = \dis evt -> do
            e :: Either SomeException () <- try $ do
              let msg = parseEvent reactor evt
              runM
                . runDiscord dis
                . runLogStdout Debug
                . runDatabase pool
                $ handleMsg reactor msg
            case e of
              Left err -> print err
              _ -> pure ()
        }
  TIO.putStrLn userFacingError
