{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE TupleSections          #-}



module Data.StateT (
      StateT(..)
    , MonadState(..)
    , lift
    , liftIO
    , evalState
    , execState
  ) where


import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Transformer

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT m) = StateT $ \s0 -> do
        (a1, s1) <- m s0
        return (f a1, s1)

instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s -> return (a, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT fn) <*> (StateT m) = StateT $ \s0 -> do
        (a1, s1) <- fn s0
        (a2, s2) <- m s1
        return (a1 a2, s2)

instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT act) >>= fn = StateT $ \s -> do
        (a1, s1) <- act s
        runStateT (fn a1) s1

instance Monad m => MonadState s (StateT s m) where
  get :: StateT s m s
  get = StateT $ \s -> return (s, s)

  put :: s -> StateT s m ()
  put s = StateT $ \_ -> return ((), s)

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift a = StateT $ \s0 -> (, s0) <$> a

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO

evalState :: Functor f => StateT s f a -> s -> f a
evalState (StateT fn) a = fst <$> fn a

execState :: Functor f => StateT s f a -> s -> f s
execState (StateT fn) a = snd <$> fn a



{-
  newtype State s a = State { runState ::  s -> (a, s) }

  instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap fn (State fn') =
      State $ \s0 ->
        let (a, s1) = fn' s0
        in (fn a, s1)

  instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s0 -> (a, s0)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State fn) <*> (State a) =
      State $ \s0 ->
        let (fn', s1) = fn s0
            (x, s2)   = a s1
        in  (fn' x, s2)

  instance Monad (State s) where
    return :: a -> State s a
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State a) >>= fn =
      State $ \s0 ->
        let (x, s1) = a s0
        in (runState $ fn x) s1
-}
