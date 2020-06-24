{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Network.Speed.InternalProp
  ( networkSpeedProps,
  )
where

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Speed.Internal
import qualified Test.Tasty as T
import Types
import qualified Test.Tasty.Hedgehog as TH

networkSpeedProps :: T.TestTree
networkSpeedProps = T.testGroup "Network.Speed.Internal" [parseSucceeds]

parseSucceeds :: T.TestTree
parseSucceeds = TH.testProperty "Parse succeeds" $ H.property $ do
  (MkBytesStr {s = s1}, MkBytesStr {s = s2}) <- H.forAll gen2BytesStr
  let res = parseAndDisplayDiff s1 s2
  H.assert $ isRSuccess res

isRSuccess :: RunResult a -> Bool
isRSuccess (RSuccess _) = True
isRSuccess _ = False

gen2BytesStr :: H.Gen (BytesStr, BytesStr)
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

genBytesStr :: H.Gen BytesStr
genBytesStr = do
  d <- Gen.integral (Range.linear 0 1_000_000_000)
  u <- Gen.integral (Range.linear 0 1_000_000_000)
  pure $ MkBytesStr d u $ show d <> " " <> show u