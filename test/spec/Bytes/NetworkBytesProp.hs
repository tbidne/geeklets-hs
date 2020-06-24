{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Bytes.NetworkBytesProp
  ( networkBytesProps,
  )
where

import Bytes.NetworkBytes
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as TH

networkBytesProps :: T.TestTree
networkBytesProps = T.testGroup "Bytes.NetworkBytes" [normalizesBytes]

normalizesBytes :: T.TestTree
normalizesBytes = TH.testProperty "Normalizes bytes" $ H.property $ do
  bytes <- H.forAll genBytes
  let bytes' = normalize bytes
  H.annotateShow bytes'
  verifyNormalized bytes bytes'

verifyNormalized :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> AnyByteSz b a -> H.PropertyT IO ()
verifyNormalized bytes (MkAnyByteSz bytes'@(MkB _)) = verifyB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkKB _)) = verifyKB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkMB _)) = verifyMB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkGB _)) = verifyGB bytes bytes'
verifyNormalized bytes (MkAnyByteSz bytes'@(MkTB _)) = verifyTB bytes bytes'

verifyB :: (Show a, Num a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'B a -> H.PropertyT IO ()
verifyB (MkB bytes) (MkB bytes') = do
  H.cover 10 "Normalized to B" True
  H.assert $ bytes < 1_000
  bytes === bytes'

verifyKB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'KB a -> H.PropertyT IO ()
verifyKB (MkB bytes) (MkKB bytes') = do
  H.cover 10 "Normalized to KB" True
  H.assert $ bytes' < 1_000
  H.annotate $ "B: " <> show bytes <> ", KB: " <> show bytes'
  eqEpsilon (bytes / 1_000) bytes' 1

verifyMB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'MB a -> H.PropertyT IO ()
verifyMB (MkB bytes) (MkMB bytes') = do
  H.cover 10 "Normalized to MB" True
  H.assert $ bytes' < 1_000
  H.annotate $ "B: " <> show bytes <> ", MB: " <> show bytes'
  eqEpsilon (bytes / 1_000_000) bytes' 1

verifyGB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'GB a -> H.PropertyT IO ()
verifyGB (MkB bytes) (MkGB bytes') = do
  H.cover 10 "Normalized to GB" True
  H.assert $ bytes' < 1_000
  H.annotate $ "B: " <> show bytes <> ", GB: " <> show bytes'
  eqEpsilon (bytes / 1_000_000_000) bytes' 1

verifyTB :: (Show a, Fractional a, Ord a) => NetworkBytes b 'B a -> NetworkBytes b 'TB a -> H.PropertyT IO ()
verifyTB (MkB bytes) (MkTB bytes') = do
  H.cover 10 "Normalized to TB" True
  H.assert $ bytes' < 1_000
  H.annotate $ "B: " <> show bytes <> ", TB: " <> show bytes'
  eqEpsilon (bytes / 1_000_000_000_000) bytes' 1

eqEpsilon :: (Show a, Num a, Ord a) => a -> a -> a -> H.PropertyT IO ()
eqEpsilon x y threshold = do
  let d = abs (x - y)
  H.footnote $ "Failed: |" <> show x <> " - " <> show y <> "| < " <> show threshold
  H.assert $ d < threshold

genBytes :: H.Gen (NetworkBytes 'Down 'B Double)
genBytes = do
  Gen.choice [genB, genKB, genMB, genGB, genTB]

genB :: H.Gen (NetworkBytes 'Down 'B Double)
genB = do
  x <- Gen.integral (Range.constantFrom 500 0 999)
  pure $ MkB (fromInteger x)

genKB :: H.Gen (NetworkBytes 'Down 'B Double)
genKB = do
  x <- Gen.integral (Range.constantFrom 500_000 1_000 999_999)
  pure $ MkB (fromInteger x)

genMB :: H.Gen (NetworkBytes 'Down 'B Double)
genMB = do
  x <- Gen.integral (Range.constantFrom 500_000_000 1_000_000 999_999_999)
  pure $ MkB (fromInteger x)

genGB :: H.Gen (NetworkBytes 'Down 'B Double)
genGB = do
  x <- Gen.integral (Range.constantFrom 500_000_000_000 1_000_000_000 999_999_999_999)
  pure $ MkB (fromInteger x)

genTB :: H.Gen (NetworkBytes 'Down 'B Double)
genTB = do
  x <- Gen.integral (Range.constantFrom 500_000_000_000_000 1_000_000_000_000 999_999_999_999_999)
  pure $ MkB (fromInteger x)