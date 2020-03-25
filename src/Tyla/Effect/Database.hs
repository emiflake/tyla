{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description: Access a PostgreSQL database with @fused-effects@
module Tyla.Effect.Database
  ( -- * Documentation
    Database (..),

    -- ** Execute queries
    runSession,

    -- ** Run effect
    runPostgreSQL,
    PostgreSQLC (..),

    -- ** Errors
    Error (..),
  )
where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Kind
import qualified Hasql.Pool as Hasql
import Hasql.Session (Session)

data Database (m :: Type -> Type) k
  = forall a. RunSession (Session a) (Either Error a -> m k)

deriving instance Functor m => Functor (Database m)

instance HFunctor Database where
  hmap f (RunSession s k) =
    RunSession s (f . k)
  {-# INLINE hmap #-}

instance Effect Database where
  thread state handler (RunSession s k) =
    RunSession s (handler . (<$ state) . k)

newtype Error
  = Error Hasql.UsageError
  deriving (Show)

runSession :: Has Database sig m => Session a -> m (Either Error a)
runSession session =
  send (RunSession session pure)

newtype PostgreSQLC m a
  = PostgreSQLC {unPostgreSQLC :: ReaderC Hasql.Pool m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Database :+: sig) (PostgreSQLC m) where
  alg = \case
    R next ->
      PostgreSQLC (alg (R (handleCoercible next)))
    L (RunSession session next) -> do
      pool <- PostgreSQLC ask
      result <- sendM $ Hasql.use pool session
      next $ first Error result

runPostgreSQL :: Hasql.Pool -> PostgreSQLC m a -> m a
runPostgreSQL pool =
  runReader pool . unPostgreSQLC
