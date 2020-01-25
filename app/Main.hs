{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import qualified Data.Text             as Text
import qualified Data.Text.IO          as TIO

import           Discord               hiding (runDiscord)
import qualified Discord

import           System.Environment

import           Modules.Counter
import           Modules.Kona
import           Modules.Logger
import           Modules.Ping

import           Control.Exception

import           Control.Effect
import           Control.Effect.Reader
import           Effect.Discord
import           Effect.Log
import           Effect.Serial

stack = sequence_
  [ ping
  , loggerModule
  , kona
  , counter
  ]

runStack dis evt = do
  e :: Either SomeException () <- try $ do
    logger <- newLogger
    runM . runSerial @"counter" @Int "cereals"
         . runDiscord dis
         . runReader evt
         . runLogStdout logger Debug
         $ stack
  case e of
    Left err -> print err
    _        -> pure ()


main :: IO ()
main = do
  Just token <- lookupEnv "DISCORD_TOKEN"
  userFacingError <- Discord.runDiscord $ def { discordToken = Text.pack token
                                      , discordOnEvent = runStack }
  TIO.putStrLn userFacingError
