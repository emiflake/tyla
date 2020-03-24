{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Config
import Data.Foldable
import Data.Functor
import Data.Text (Text)
import qualified Discord.Internal.Types.Channel as Discord
import qualified Discord.Types as Discord
import qualified Tyla.Effect.Discord as Discord
import qualified Tyla.Parser as Parser
import Tyla.Parser (Parser)
import Tyla.Prelude hiding (handleMsg, parseEvent)
import qualified Tyla.Prelude as Tyla

data Command
  = Ping Discord.ChannelId
  | Succ Discord.ChannelId Int
  deriving (Show)

data Msg
  = Message Command
  | NoOp

prefixed :: Parser a -> Parser a
prefixed = (Parser.text "\\" *>)

parseCommand :: Discord.Message -> Parser Command
parseCommand Discord.Message {..} =
  prefixed $
    asum
      [ Parser.text "ping"
          $> Ping messageChannel,
        Parser.text "succ"
          *> Parser.whitespace
          *> Parser.int
          <&> Succ messageChannel
      ]

parseEvent :: Discord.Event -> Msg
parseEvent evt =
  case evt of
    Discord.MessageCreate msg ->
      case Parser.runMaybe (parseCommand msg) (Discord.messageText msg) of
        Just command -> Message command
        Nothing -> NoOp
    _ -> NoOp

handleMsg :: Tyla.Stack sig m => Msg -> m ()
handleMsg msg =
  case msg of
    NoOp ->
      pure ()
    Message command ->
      case command of
        Ping channelId ->
          void $ Discord.sendMessage channelId "Hello"
        Succ channelId number ->
          void $
            Discord.sendMessage
              channelId
              (showText (succ number))

main :: IO ()
main = do
  Just config <- Config.read "config.json"
  Tyla.run (Config.configToken config) $
    Tyla.Reactor
      { Tyla.parseEvent = parseEvent,
        Tyla.handleMsg = handleMsg
      }
