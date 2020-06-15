{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Network.Speed.Internal
  ( absDiff,
    parseAndDisplayDiff,
  )
where

import Bytes.NetworkBytes
import Bytes.NetworkBytesPair
import Common
import Control.Applicative (liftA2)
import Types

absDiff :: (Applicative f, Num a) => f a -> f a -> f a
absDiff x = (<$>) abs . liftA2 (-) x

parseAndDisplayDiff :: String -> String -> RunResultStr
parseAndDisplayDiff s1 s2 = dispSpeed <$> absDiff (parseRaw s1) (parseRaw s2)

parseRaw :: String -> RunResult (NetworkBytesPair 'B 'B Integer)
parseRaw s =
  let (_, _, _, matches) = s `capture` regex
   in case matches of
        [downStr, upStr] -> readPair downStr upStr MkB
        _ -> RFailure $ "Incorrectly formatted: " <> show matches

regex :: String
regex = "([0-9]+) ([0-9]+)"

dispSpeed ::  NetworkBytesPair s t Integer -> String
dispSpeed (MkBytesPair (d, u)) =
  let d' = normalize $ fmap (fromInteger @Double) d
      u' = normalize $ fmap (fromInteger @Double) u
   in "Down: "
        <> dispAnyBytes d'
        <> "/s\n"
        <> "Up:     "
        <> dispAnyBytes u'
        <> "/s"