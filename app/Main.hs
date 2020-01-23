{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
import           Control.Concurrent (threadDelay)
import           Control.Monad      (guard, when)
import qualified Data.Text          as Text
import qualified Data.Text.IO       as TIO

import           Discord
import qualified Discord.Requests   as R
import           Discord.Types

import           System.Environment

import           Data.HList

import           Command

import           Data.IORef

main :: IO ()
main = do
  ref <- newIORef 0
  let subs = Kona
          :# Ping
          :# Counter ref
          :# HNil

  Just token <- lookupEnv "DISCORD_TOKEN"
  userFacingError <- runDiscord $ def { discordToken = Text.pack token
                                      , discordOnEvent = handle subs }
  TIO.putStrLn userFacingError
