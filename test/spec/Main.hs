module Main
  ( main,
  )
where

import Bytes.NetworkBytesProp
import Bytes.NetworkBytesSpec
import Network.IP.InternalProp
import Network.Speed.InternalProp
import qualified Test.Tasty as T
import TypesSpec

main :: IO ()
main = specs >>= \s -> T.defaultMain $ T.testGroup "Tests" [props, s]

specs :: IO T.TestTree
specs =
  (\t1 t2 -> T.testGroup "HSpec Specs" (t1 <> t2))
    <$> networkBytesSpecs
    <*> dslSpec

props :: T.TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ networkBytesProps,
      networkIpProps,
      networkSpeedProps
    ]