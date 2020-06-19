{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Bytes.NetworkBytesSpec
  ( spec_networkBytesSpec,
  )
where

import Bytes.NetworkBytes
import Test.Hspec

spec_networkBytesSpec :: Spec
spec_networkBytesSpec = do
  describe "Bytes.NetworkBytesSpec" $ do
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