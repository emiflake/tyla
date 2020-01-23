{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Booru.Kona where

import qualified Data.Text           as Text
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client      hiding (manager)

import           Data.Aeson
import           Data.Proxy          (Proxy (..))

import           Text.Pretty.Simple

import           Data.List
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           Data.Ord

newtype Tag =
  Tag String
  deriving (Show, Eq, Ord)

genTags :: String -> [Tag]
genTags = fmap Tag . words

data KonaPost = KonaPost
  { _id      :: Int
  , _tags    :: [Tag]
  , _fileUrl :: String
  }
  deriving Show

instance FromJSON KonaPost where
  parseJSON = withObject "KonaPost" $ \v ->
    KonaPost <$> v .: "id"
             <*> (genTags <$> v .: "tags")
             <*> v .: "file_url"

type KonaAPI =
  "post.json" :> QueryParam' '[Required] "page" Int
              :> QueryParam' '[Required] "limit" Int
              :> QueryParam' '[Required] "tags" [String]
              :> Get '[JSON] [KonaPost]

api :: Proxy KonaAPI
api = Proxy

instance ToHttpApiData [String] where
  toQueryParam = Text.pack . unwords

getPosts :: Int -> Int -> [String] -> ClientM [KonaPost]
getPosts = client api

doGetPosts :: Int -> Int -> [String] -> IO (Maybe [KonaPost])
doGetPosts page limit tags = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (getPosts page limit tags) (mkClientEnv manager (BaseUrl Http "konachan.com" 80 ""))
  case res of
    Right v -> pure . pure $ v
    Left _  -> pure Nothing

increment :: Ord k => Map k Integer -> k -> Map k Integer
increment m k = Map.alter (Just . succ . fromMaybe 0) k m

example :: IO ()
example = do
  let accumulate :: [KonaPost] -> Map Tag Integer -> Map Tag Integer
      accumulate posts acc = foldr (flip increment) acc (_tags =<< posts)
      loop acc p = do
        posts <- doGetPosts p 1000 []
        case posts of
          Just posts' -> pPrint (take 10 . sortOn (Down . snd) $ Map.toList acc)
                      >> loop (accumulate posts' acc) (succ p)
          Nothing    -> pure ()
  loop Map.empty 0
