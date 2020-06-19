{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}

module Bytes.NetworkBytesProp
  ( hprop_normalizes,
  )
where

import Bytes.NetworkBytes
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hprop_normalizes :: Property
hprop_normalizes = property $ do
  bytes <- forAll genBytes
  let bytes' = normalize bytes
  annotateShow bytes'
  verifyNormalized bytes bytes'

verifyNormalized :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> AnyByteSz b a -> PropertyT IO ()
verifyNormalized bytes (MkAnyByteSz bytes'@(MkB _)) = verifyB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkKB _)) = verifyKB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkMB _)) = verifyMB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkGB _)) = verifyGB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkTB _)) = verifyTB bytes bytes'

verifyB :: (Show a, Num a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'B a -> PropertyT IO ()
verifyB (MkB bytes) (MkB bytes') = do
  assert $ bytes < 1_000
  bytes === bytes'

verifyKB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'KB a -> PropertyT IO ()
verifyKB (MkB bytes) (MkKB bytes') = do
  assert $ bytes' < 1_000
  annotate $ "B: " <> show bytes <> ", KB: " <> show bytes'
  eqEpsilon (bytes / 1_000) bytes' 1

verifyMB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'MB a -> PropertyT IO ()
verifyMB (MkB bytes) (MkMB bytes') = do
  assert $ bytes' < 1_000
  annotate $ "B: " <> show bytes <> ", MB: " <> show bytes'
  eqEpsilon (bytes / 1_000_000) bytes' 1

verifyGB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'GB a -> PropertyT IO ()
verifyGB (MkB bytes) (MkGB bytes') = do
  assert $ bytes' < 1_000
  annotate $ "B: " <> show bytes <> ", GB: " <> show bytes'
  eqEpsilon (bytes / 1_000_000_000) bytes' 1

verifyTB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'TB a -> PropertyT IO ()
verifyTB (MkB bytes) (MkTB bytes') = do
  assert $ bytes' < 1_000
  annotate $ "B: " <> show bytes <> ", TB: " <> show bytes'
  eqEpsilon (bytes / 1_000_000_000_000) bytes' 1

eqEpsilon :: (Show a, Num a, Ord a) => a -> a -> a -> PropertyT IO ()
eqEpsilon x y threshold = do
  let d = abs (x - y)
  footnote $ "Failed: |" <> show x <> " - " <> show y <> "| < " <> show threshold
  assert $ d < threshold

genBytes :: Gen (NetworkBytes 'Down 'B Double)
genBytes = do
  Gen.choice [genB, genKB, genMB, genGB, genTB]

genB :: Gen (NetworkBytes 'Down 'B Double)
genB = do
  x <- Gen.integral (Range.constantFrom 500 0 999)
  pure $ MkB (fromInteger x)

genKB :: Gen (NetworkBytes 'Down 'B Double)
genKB = do
  x <- Gen.integral (Range.constantFrom 500_000 1_000 999_999)
  pure $ MkB (fromInteger x)

genMB :: Gen (NetworkBytes 'Down 'B Double)
genMB = do
  x <- Gen.integral (Range.constantFrom 500_000_000 1_000_000 999_999_999)
  pure $ MkB (fromInteger x)

genGB :: Gen (NetworkBytes 'Down 'B Double)
genGB = do
  x <- Gen.integral (Range.constantFrom 500_000_000_000 1_000_000_000 999_999_999_999)
  pure $ MkB (fromInteger x)

genTB :: Gen (NetworkBytes 'Down 'B Double)
genTB = do
  x <- Gen.integral (Range.constantFrom 500_000_000_000_000 1_000_000_000_000 999_999_999_999_999)
  pure $ MkB (fromInteger x)