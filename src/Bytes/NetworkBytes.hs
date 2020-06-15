{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Bytes.NetworkBytes
  ( AnyByteSz (..),
    ByteDir (..),
    NetworkBytes (..),
    dispAnyBytes,
    mapAnySz,
    normalize,
    normalizeAny,
    readBytes,
    unNetworkBytes,
    module Bytes.Size,
  )
where

import Bytes.Size
import qualified Text.Printf as Pf
import Common
import Types

data ByteDir
  = Up
  | Down
  deriving (Eq, Show)

data NetworkBytes (b :: ByteDir) (s :: ByteSz) a where
  MkB :: a -> NetworkBytes b 'B a
  MkKB :: a -> NetworkBytes b 'KB a
  MkMB :: a -> NetworkBytes b 'MB a
  MkGB :: a -> NetworkBytes b 'GB a
  MkTB :: a -> NetworkBytes b 'TB a

instance Functor (NetworkBytes b s) where
  fmap f (MkB x) = MkB $ f x
  fmap f (MkKB x) = MkKB $ f x
  fmap f (MkMB x) = MkMB $ f x
  fmap f (MkGB x) = MkGB $ f x
  fmap f (MkTB x) = MkTB $ f x

instance Show a => Show (NetworkBytes 'Up s a) where
  show (MkB a) = "B Up " <> show a
  show (MkKB a) = "KB Up " <> show a
  show (MkMB a) = "MB Up " <> show a
  show (MkGB a) = "GB Up " <> show a
  show (MkTB a) = "TB Up " <> show a

instance Show a => Show (NetworkBytes 'Down s a) where
  show (MkB a) = "B Down " <> show a
  show (MkKB a) = "KB Down " <> show a
  show (MkMB a) = "MB Down " <> show a
  show (MkGB a) = "GB Down " <> show a
  show (MkTB a) = "TB Down " <> show a

instance Eq a => Eq (NetworkBytes b s a) where
  x == y = (unNetworkBytes x) == (unNetworkBytes y)

instance Ord a => Ord (NetworkBytes b s a) where
  x <= y = (unNetworkBytes x) <= (unNetworkBytes y)

instance Num a => Num (NetworkBytes b 'B a) where
  (MkB x) + (MkB y) = MkB $ x + y
  (MkB x) - (MkB y) = MkB $ x - y
  (MkB x) * (MkB y) = MkB $ x * y
  abs (MkB x) = MkB $ abs x
  signum (MkB x) = MkB $ signum x
  fromInteger = MkB . fromInteger

instance Num a => Num (NetworkBytes b 'KB a) where
  (MkKB x) + (MkKB y) = MkKB $ x + y
  (MkKB x) - (MkKB y) = MkKB $ x - y
  (MkKB x) * (MkKB y) = MkKB $ x * y
  abs (MkKB x) = MkKB $ abs x
  signum (MkKB x) = MkKB $ signum x
  fromInteger = MkKB . fromInteger

instance Num a => Num (NetworkBytes b 'MB a) where
  (MkMB x) + (MkMB y) = MkMB $ x + y
  (MkMB x) - (MkMB y) = MkMB $ x - y
  (MkMB x) * (MkMB y) = MkMB $ x * y
  abs (MkMB x) = MkMB $ abs x
  signum (MkMB x) = MkMB $ signum x
  fromInteger = MkMB . fromInteger

instance Num a => Num (NetworkBytes b 'GB a) where
  (MkGB x) + (MkGB y) = MkGB $ x + y
  (MkGB x) - (MkGB y) = MkGB $ x - y
  (MkGB x) * (MkGB y) = MkGB $ x * y
  abs (MkGB x) = MkGB $ abs x
  signum (MkGB x) = MkGB $ signum x
  fromInteger = MkGB . fromInteger

instance Num a => Num (NetworkBytes b 'TB a) where
  (MkTB x) + (MkTB y) = MkTB $ x + y
  (MkTB x) - (MkTB y) = MkTB $ x - y
  (MkTB x) * (MkTB y) = MkTB $ x * y
  abs (MkTB x) = MkTB $ abs x
  signum (MkTB x) = MkTB $ signum x
  fromInteger = MkTB . fromInteger

instance Fractional a => IncByteSz (NetworkBytes b 'B a) where
  type Next (NetworkBytes b 'B a) = NetworkBytes b 'KB a
  next (MkB x) = MkKB $ x / 1_000

instance Fractional a => IncByteSz (NetworkBytes b 'KB a) where
  type Next (NetworkBytes b 'KB a) = NetworkBytes b 'MB a
  next (MkKB x) = MkMB $ x / 1_000

instance Fractional a => IncByteSz (NetworkBytes b 'MB a) where
  type Next (NetworkBytes b 'MB a) = NetworkBytes b 'GB a
  next (MkMB x) = MkGB $ x / 1_000

instance Fractional a => IncByteSz (NetworkBytes b 'GB a) where
  type Next (NetworkBytes b 'GB a) = NetworkBytes b 'TB a
  next (MkGB x) = MkTB $ x / 1_000

unNetworkBytes :: NetworkBytes b s a -> a
unNetworkBytes (MkB x) = x
unNetworkBytes (MkKB x) = x
unNetworkBytes (MkMB x) = x
unNetworkBytes (MkGB x) = x
unNetworkBytes (MkTB x) = x

data AnyByteSz b a where
  MkAnyByteSz :: NetworkBytes b s a -> AnyByteSz b a

instance Show a => Show (AnyByteSz 'Down a) where
  show (MkAnyByteSz b) = "MkAnyByteSz " <> show b

instance Show a => Show (AnyByteSz 'Up a) where
  show (MkAnyByteSz b) = "MkAnyByteSz " <> show b

instance Functor (AnyByteSz b) where
  fmap f (MkAnyByteSz x) = MkAnyByteSz (fmap f x)

mapAnySz :: (forall s. NetworkBytes b s a -> NetworkBytes c t d) -> AnyByteSz b a -> AnyByteSz c d
mapAnySz f (MkAnyByteSz x) = MkAnyByteSz (f x)

dispAnyBytes :: Pf.PrintfArg a => AnyByteSz b a -> String
dispAnyBytes (MkAnyByteSz (MkB x)) = Pf.printf "%.2f" x <> " B"
dispAnyBytes (MkAnyByteSz (MkKB x)) = Pf.printf "%.2f" x <> " KB"
dispAnyBytes (MkAnyByteSz (MkMB x)) = Pf.printf "%.2f" x <> " MB"
dispAnyBytes (MkAnyByteSz (MkGB x)) = Pf.printf "%.2f" x <> " GB"
dispAnyBytes (MkAnyByteSz (MkTB x)) = Pf.printf "%.2f" x <> " TB"

normalize :: (Fractional a, Ord a) => NetworkBytes b s a -> AnyByteSz b a
normalize b@(MkB x)
  | x > 999 = normalize $ next b
  | otherwise = MkAnyByteSz b
normalize b@(MkKB x)
  | x > 999 = normalize $ next b
  | otherwise = MkAnyByteSz b
normalize b@(MkMB x)
  | x > 999 = normalize $ next b
  | otherwise = MkAnyByteSz b
normalize b@(MkGB x)
  | x > 999 = normalize $ next b
  | otherwise = MkAnyByteSz b
normalize b@(MkTB _) = MkAnyByteSz b

normalizeAny :: (Fractional a, Ord a) => AnyByteSz b a -> AnyByteSz b a
normalizeAny (MkAnyByteSz x) = normalize x

readBytes ::
  Read a =>
  String ->
  (a -> NetworkBytes b s a) ->
  RunResult (NetworkBytes b s a)
readBytes str cons = cons <$> readResult str