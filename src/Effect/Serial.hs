{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Effect.Serial
  ( Serial(..)
  , SerialC
  , runSerial
  , store
  , load
  ) where

import           Prelude                hiding (log)

import           Codec.Serialise
import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Exception
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Proxy
import           GHC.TypeLits

data Serial (name :: Symbol) value (m :: * -> *) k where
  Store :: Serialise value
        => value
        -> (() -> m k)
        -> Serial name value m k
  Load  :: Serialise value
        => (Maybe value -> m k)
        -> Serial name value m k

store :: forall name value (m :: * -> *) sig.
         ( Serialise value
         , Carrier sig m
         , Member (Serial (name :: Symbol) value) sig
         ) => value -> m ()
store val = send (Store val pure :: Serial name value m ())

load :: forall name value (m :: * -> *) sig.
         ( Serialise value
         , Carrier sig m
         , Member (Serial (name :: Symbol) value) sig
         ) => m (Maybe value)
load = send (Load pure :: Serial name value m (Maybe value))

deriving instance Functor m => Functor (Serial name value m)

instance HFunctor (Serial name value) where
  hmap f (Store value k) = Store value (f . k)
  hmap f (Load k)        = Load (f . k)

instance Effect (Serial name a) where
  handle state handler (Store value k) =
    Store value (handler . (<$ state) . k)
  handle state handler (Load k) =
    Load ((handler . (<$ state)) . k)

newtype SerialC (name :: Symbol) v m a =
  SerialC { unSerialC :: ReaderC FilePath m a } -- TODO: add config to reader

  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance ( Serialise value
         , KnownSymbol name
         , Carrier sig m
         , MonadIO m
         ) => Carrier (Serial name value :+: sig) (SerialC name value m) where
  eff c =
    case c of
      L op -> do
        let (</>) a b = a <> "/" <> b
        (path :: FilePath) <- SerialC ask
        case op of
          Store val next -> do
            liftIO $ writeFileSerialise (path </> symbolVal (Proxy :: Proxy name)) val
            next ()
          Load next -> do
            val <- liftIO $ do
              res <- try $ readFileDeserialise (path </> symbolVal (Proxy :: Proxy name)) :: IO (Either DeserialiseFailure value)
              case res of
                Left _  -> pure Nothing
                Right v -> pure (Just v)
            next val
      R next ->
        SerialC (eff (R (handleCoercible next)))

runSerial :: forall name value m a. FilePath -> SerialC name value m a -> m a
runSerial fp =
  runReader fp . unSerialC
