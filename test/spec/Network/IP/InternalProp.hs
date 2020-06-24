{-# LANGUAGE DataKinds #-}

module Network.IP.InternalProp
  ( networkIpProps,
  )
where

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.IP.Internal
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as TH

networkIpProps :: T.TestTree
networkIpProps = T.testGroup "Network.IP.Internal" [combinedIPsMatch]

combinedIPsMatch :: T.TestTree
combinedIPsMatch = TH.testProperty "Combines IPs" $ H.property $ do
  (local@(MkIP l), global@(MkIP g)) <- H.forAll genIPs
  "Local  IP: "
    <> l
    <> "\nGlobal IP: "
    <> g
    === combineIPs local global

genIPs :: H.Gen (IP 'Local, IP 'Global)
genIPs = (,) <$> genIP <*> genIP
  where
    genStr = Gen.string (Range.linear 0 100) Gen.ascii
    genIP = MkIP <$> genStr