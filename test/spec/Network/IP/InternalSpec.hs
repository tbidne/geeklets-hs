{-# LANGUAGE DataKinds #-}

module Network.IP.InternalSpec
  ( hprop_combineIPs,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import Network.IP.Internal
import qualified Hedgehog.Range as Range

hprop_combineIPs :: Property
hprop_combineIPs = property $ do
  (local@(MkIP l), global@(MkIP g)) <- forAll genIPs
  "Local  IP: "
    <> l
    <> "\nGlobal IP: "
    <> g
    === combineIPs local global

genIPs :: Gen (IP 'Local, IP 'Global)
genIPs = (,) <$> genIP <*> genIP
  where genStr = Gen.string (Range.linear 0 100) Gen.ascii
        genIP = MkIP <$> genStr