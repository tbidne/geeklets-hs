{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Network.Total
  ( getTotal,
  )
where

import Common
import IOUtils
import Text.Printf as TP

import Bytes

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
          let bytes = readBytesPair downNum upNum
          in fmap (dispBytes (formatDown downSfx) (formatUp upSfx)) bytes
        _ -> RFailure $ "Incorrectly formatted: " <> show matches

regex :: String
regex = "[0-9]+\\/([0-9]+)([KMG]) [0-9]+\\/([0-9]+)([KMG])"

formatDown :: String -> Bytes 'Down Float -> String
formatDown sfx (MkBytes d) = "Downloaded: " <> dispFloat d sfx

formatUp :: String -> Bytes 'Up Float -> String
formatUp sfx (MkBytes u) = "Uploaded:     " <> dispFloat u sfx

dispFloat :: Float -> String -> String
dispFloat x sfx
  | x > 999 = rnd x sfx
  | otherwise = show x <> sfx
  where rnd n = TP.printf "%.2f" (n / 1_000)
          <> (\case
               "K" -> "M"
               "M" -> "G"
               "G" -> "T"
               t -> t)