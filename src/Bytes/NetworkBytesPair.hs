{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Bytes.NetworkBytesPair
  ( NetworkBytesPair (..),
    readPair,
  )
where

import Common
import Bytes.NetworkBytes
import Types

newtype NetworkBytesPair s t a = MkBytesPair (NetworkBytes 'Down s a, NetworkBytes 'Up t a)

instance Show a => Show (NetworkBytesPair s t a) where
  show (MkBytesPair (x, y)) = "MkBytesPair (" <> show x <> "," <> show y <> ")"

instance Eq a => Eq (NetworkBytesPair s t a) where
  (MkBytesPair (x, y)) == (MkBytesPair (x', y')) = x == x' && y == y'

instance
  (Num a, Num (NetworkBytes 'Down s a), Num (NetworkBytes 'Up t a)) =>
  Num (NetworkBytesPair s t a)
  where
  (MkBytesPair (d1, u1)) + (MkBytesPair (d2, u2)) = MkBytesPair (d1 + d2, u1 + u2)
  (MkBytesPair (d1, u1)) - (MkBytesPair (d2, u2)) = MkBytesPair (d1 - d2, u1 - u2)
  (MkBytesPair (d1, u1)) * (MkBytesPair (d2, u2)) = MkBytesPair (d1 * d2, u1 * u2)
  abs (MkBytesPair (d, u)) = MkBytesPair (abs d, abs u)
  signum (MkBytesPair (d, u)) = MkBytesPair (signum d, signum u)
  fromInteger x = MkBytesPair (fromInteger x, fromInteger x)

readPair ::
  Read a =>
  String ->
  String ->
  (forall b. a -> NetworkBytes b s a) ->
  RunResult (NetworkBytesPair s s a)
readPair downStr upStr cons =
  let d' = readResult downStr
      u' = readResult upStr
   in (\d u -> MkBytesPair (cons d, cons u)) <$> d' <*> u'