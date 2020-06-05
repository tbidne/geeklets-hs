{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Freer
  ( Freer (..),
    etaF,
    foldFreer,
  )
where

import Control.Monad ((>=>))

-- Based on the paper "Freer Monads, More Extensible Effects"
-- - http://okmij.org/ftp/Haskell/extensible/more.pdf
-- - http://okmij.org/ftp/Computation/free-monad.html
data Freer f a where
  Pure :: a -> Freer f a
  Impure :: f x -> (x -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap :: (a -> b) -> Freer f a -> Freer f b
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure x g) = Impure x (fmap f . g)

instance Applicative (Freer f) where
  pure :: a -> Freer f a
  pure = Pure

  (<*>) :: Freer f (a -> b) -> Freer f a -> Freer f b
  Pure f <*> x = fmap f x
  Impure u q <*> x = Impure u ((<*> x) . q)

instance Monad (Freer f) where
  (>>=) :: Freer f a -> (a -> Freer f b) -> Freer f b
  Pure x >>= k = k x
  Impure u k' >>= k = Impure u (k' >=> k)

foldFreer :: Monad m => (forall x. i x -> m x) -> Freer i a -> m a
foldFreer _ (Pure x) = pure x
foldFreer eta (Impure i f) = eta i >>= (foldFreer eta . f)

etaF :: f a -> Freer f a
etaF x = Impure x Pure
