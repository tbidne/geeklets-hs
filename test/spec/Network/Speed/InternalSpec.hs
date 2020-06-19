{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Network.Speed.InternalSpec
  ( hprop_parseSucceeds,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Speed.Internal
import Types

hprop_parseSucceeds :: Property
hprop_parseSucceeds = property $ do
  (MkBytesStr {s = s1}, MkBytesStr {s = s2}) <- forAll gen2BytesStr
  let res = parseAndDisplayDiff s1 s2
  assert $ isRSuccess res

isRSuccess :: RunResult a -> Bool
isRSuccess (RSuccess _) = True
isRSuccess _ = False

newtype TwoBytesStr = TwoBytesStr ((Integer, Integer, String), (Integer, Integer, String))
  deriving (Eq, Show)

gen2BytesStr :: Gen (BytesStr, BytesStr)
gen2BytesStr = do
  x <- genBytesStr
  y <- genBytesStr
  pure $ (x, y)

data BytesStr = MkBytesStr
  { down :: Integer,
    up :: Integer,
    s :: String
  }
  deriving (Show)

genBytesStr :: Gen BytesStr
genBytesStr = do
  d <- Gen.integral (Range.linear 0 1_000_000_000)
  u <- Gen.integral (Range.linear 0 1_000_000_000)
  pure $ MkBytesStr d u $ show d <> " " <> show u