{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Effect.Database
  (
  ) where

-- import           Control.Effect.Carrier
-- import           Control.Effect.Reader


-- data Database (m :: * -> *) k
--   = forall a. Database (Session a) (Either Error a -> m k)


-- deriving instance Functor m => Functor (Database m)


-- instance HFunctor Database where
--   hmap f (Database s k) = Database s (f . k)
--   {-# INLINE hmap #-}


-- instance Effect Database where
--   handle state handler (Database s k) = Database s (handler . (<$ state) . k)


-- newtype Error
--   = DatabaseError Hasql.UsageError
--   deriving (Show)


-- runSession :: (Member Database sig, Carrier sig m) => Session a -> m (Either Error a)
-- runSession session =
--   send (Database session pure)


-- newtype DatabaseC m a =
--   DatabaseC { unDBC :: ReaderC Hasql.Pool m a}
--   deriving newtype (Applicative, Functor, Monad, MonadIO)


-- instance (Carrier sig m, MonadIO m) => Carrier (Database :+: sig) (DatabaseC m) where
--   eff c =
--     case c of
--       L (Database session next) -> do
--         pool <- DatabaseC ask
--         result <- liftIO $ Hasql.use pool session
--         case result of
--           Left e ->
--             next . Left $ DatabaseError e
--           Right ok ->
--             next $ Right ok
--       R next ->
--         DatabaseC (eff (R (handleCoercible next)))


-- runDatabase :: Hasql.Pool -> DatabaseC m a -> m a
-- runDatabase pool =
--   runReader pool . unDBC
