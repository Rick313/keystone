{-# LANGUAGE InstanceSigs #-}

module Control.Monad.Transformer (
    MonadTrans(..)
  , MonadUnliftIO(..)
  ) where

import           Control.Monad.IO.Class (MonadIO)

class MonadTrans t where
  lift :: Monad m => m a -> t m a

class MonadIO m => MonadUnliftIO m where
  askRunInIO :: m (m a -> IO a)

instance MonadUnliftIO IO where
  askRunInIO :: IO (IO a -> IO a)
  askRunInIO = return id

