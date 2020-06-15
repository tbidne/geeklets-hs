{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Network.Total
  ( getTotal,
  )
where

import Bytes.NetworkBytes
import Common
import Control.Applicative (liftA2)
import IOUtils

getTotal :: IO RunResultStr
getTotal = rawTotal >>= (pure . (=<<) parseRaw)

rawTotal :: IO RunResultStr
rawTotal =
  runCmd
    "top -l5\
    \ | grep \"Network\"\
    \ | tail -1\
    \ | awk \'{print $3, $5}\'"

parseRaw :: String -> RunResultStr
parseRaw s =
  let (_, _, _, matches) = s `capture` regex
   in case matches of
        [downNum, downSfx, upNum, upSfx] ->
          let d' = readAndNormalize downNum downSfx
              u' = readAndNormalize upNum upSfx
           in liftA2 dispTotal d' u'
        _ -> RFailure $ "Incorrectly formatted: " <> show matches

regex :: String
regex = "[0-9]+\\/([0-9]+)([KMG]) [0-9]+\\/([0-9]+)([KMG])"

readAndNormalize :: String -> String -> RunResult String
readAndNormalize num suffix = dispAnyBytes . normalizeAny <$> readWithSuffix @Float num suffix

readWithSuffix :: Read a => String -> String -> RunResult (AnyByteSz b a)
readWithSuffix s "" = MkAnyByteSz <$> readBytes s MkB
readWithSuffix s "K" = MkAnyByteSz <$> readBytes s MkKB
readWithSuffix s "M" = MkAnyByteSz <$> readBytes s MkMB
readWithSuffix s "G" = MkAnyByteSz <$> readBytes s MkGB
readWithSuffix s "T" = MkAnyByteSz <$> readBytes s MkTB
readWithSuffix _ x = RFailure $ "Bad byte suffix found: " <> x

dispTotal :: String -> String -> String
dispTotal down up =
  "Downloaded: "
    <> down
    <> "\nUploaded:     "
    <> up