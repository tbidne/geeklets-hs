{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}

module Bytes.NetworkBytesSpec
  ( networkBytesSpecs,
  )
where

import Bytes.NetworkBytes
import Test.Hspec
import qualified Test.Tasty as T
import qualified Test.Tasty.Hspec as TH

networkBytesSpecs :: IO [T.TestTree]
networkBytesSpecs = TH.testSpecs $ do
  describe "Bytes.NetworkBytes" $ do
    it "Displays B" $ do
      dispAnyBytes mkB `shouldBe` "5.25 B"
    it "Displays KB" $ do
      dispAnyBytes mkKB `shouldBe` "421.00 KB"
    it "Displays MB" $ do
      dispAnyBytes mkMB `shouldBe` "62.41 MB"
    it "Displays GB" $ do
      dispAnyBytes mkGB `shouldBe` "12.20 GB"
    it "Displays TB" $ do
      dispAnyBytes mkTB `shouldBe` "7.00 TB"

mkB :: AnyByteSz 'Up Float
mkB = MkAnyByteSz $ MkB 5.2493

mkKB :: AnyByteSz 'Down Float
mkKB = MkAnyByteSz $ MkKB 421.00

mkMB :: AnyByteSz 'Up Float
mkMB = MkAnyByteSz $ MkMB 62.407

mkGB :: AnyByteSz 'Down Float
mkGB = MkAnyByteSz $ MkGB 12.2

mkTB :: AnyByteSz 'Up Float
mkTB = MkAnyByteSz $ MkTB 7