{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Text             as Text
import qualified Data.Text.IO          as TIO

import           Discord               hiding (runDiscord)
import qualified Discord

import           System.Environment

import           Modules.Kona
import           Modules.Logger
import           Modules.Ping

import           Control.Exception

import           Control.Effect
import           Control.Effect.Reader
import           Effect.Discord
import           Effect.Log

stack = sequence_
  [ ping
  , loggerModule
  , kona
  ]

runStack dis evt = do
  e :: Either SomeException () <- try $ do
    logger <- newLogger
    runM . runDiscord dis . runReader evt . runLogStdout logger Debug $ stack
  case e of
    Left err -> print err
    _        -> pure ()


main :: IO ()
main = do
  Just token <- lookupEnv "DISCORD_TOKEN"
  userFacingError <- Discord.runDiscord $ def { discordToken = Text.pack token
                                      , discordOnEvent = runStack }
  TIO.putStrLn userFacingError
