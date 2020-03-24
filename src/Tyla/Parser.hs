module Tyla.Parser
  ( module Text.Parsec,
    module Text.Parsec.Text,
    run,
    runMaybe,
    whitespace,
    int,
    text,
  )
where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec
import Text.Parsec.Text

-- PRIMITIVES

-- TODO: Optimize
text :: Text -> Parser Text
text match =
  Text.pack <$> string (Text.unpack match)

whitespace :: Parser ()
whitespace =
  void . many $ oneOf " "

int :: Parser Int
int =
  read <$> many1 digit

-- RUNNER

run :: Parser a -> Text -> Either ParseError a
run parser input =
  runParser parser () "" input

runMaybe :: Parser a -> Text -> Maybe a
runMaybe parser input =
  case run parser input of
    Left _ -> Nothing
    Right v -> Just v
