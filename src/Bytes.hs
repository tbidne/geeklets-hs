{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Bytes
  ( ByteType (..),
    Bytes (..),
    BytesPair (..),
    dispBytes,
    readBytesPair,
  )
where

import Common
import Types

data ByteType
  = Up
  | Down

newtype Bytes (b :: ByteType) a = MkBytes a
  deriving (Eq, Show, Num)

newtype BytesPair a = MkBytesPair (Bytes 'Down a, Bytes 'Up a)
  deriving (Eq, Show)

instance Num a => Num (BytesPair a) where
  (MkBytesPair (d1, u1)) + (MkBytesPair (d2, u2)) = MkBytesPair (d1 + d2, u1 + u2)
  (MkBytesPair (d1, u1)) - (MkBytesPair (d2, u2)) = MkBytesPair (d1 - d2, u1 - u2)
  (MkBytesPair (d1, u1)) * (MkBytesPair (d2, u2)) = MkBytesPair (d1 * d2, u1 * u2)
  abs (MkBytesPair (d, u)) = MkBytesPair (abs d, abs u)
  signum (MkBytesPair (d, u)) = MkBytesPair (signum d, signum u)
  fromInteger x = MkBytesPair (fromInteger x, fromInteger x)

readBytesPair :: Read a => String -> String -> RunResult (BytesPair a)
readBytesPair downStr upStr =
  let d' = readResult downStr
      u' = readResult upStr
   in (\d u -> MkBytesPair (MkBytes d, MkBytes u)) <$> d' <*> u'

dispBytes ::
  (Bytes 'Down a -> String) ->
  (Bytes 'Up a -> String) ->
  BytesPair a ->
  String
dispBytes downFn upFn (MkBytesPair (d, u)) =
  downFn d
    <> "\n"
    <> upFn u