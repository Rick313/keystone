{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.EitherT (
    EitherT(..)
  , left
  , right
  , lift
  , liftEither
  , liftIO
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Transformer

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap fn (EitherT m) = EitherT $ fmap fn <$> m

instance Monad m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure $ Right a

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT fn) <*> (EitherT a) =
    EitherT $ apply =<< fn
    where apply (Left e)    = return $ Left e
          apply (Right fn') = fmap fn' <$> a

instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT a) >>= fn =
    EitherT $ apply =<< a
    where apply (Left e)   = return $ Left e
          apply (Right a') = runEitherT $ fn a'

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift a = EitherT $ Right <$> a

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  liftIO = lift . liftIO

left :: Monad m => e -> EitherT e m a
left = EitherT . return . Left

right :: Monad m => a -> EitherT e m a
right = EitherT . return . Right

liftEither :: Monad m => Either e a -> EitherT e m a
liftEither = EitherT . return
