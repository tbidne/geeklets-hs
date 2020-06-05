{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Network.Total
  ( getTotal,
  )
where

import Common
import IOUtils
import Text.Printf as TP
import Text.Read as TR

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
          let down = resToDisp downNum downSfx
              up = resToDisp upNum upSfx
              totals = (,) <$> down <*> up
           in fmap combineTotals totals
        _ -> RFailure $ "Incorrectly formatted: " <> show matches

regex :: String
regex = "[0-9]+\\/([0-9]+)([KMG]) [0-9]+\\/([0-9]+)([KMG])"

combineTotals :: (String, String) -> String
combineTotals (down, up) =
  "Downloaded: "
    <> down
    <> "\nUploaded:     "
    <> up

resToDisp :: String -> String -> RunResultStr
resToDisp num sfx =
  case TR.readMaybe @Float num of
    Just n
      | n > 999 -> RSuccess $ rnd n sfx
      | otherwise -> RSuccess $ num <> sfx
    Nothing -> RFailure $ "Could not parse num " <> num <> " and suffix " <> sfx

rnd :: Float -> String -> String
rnd x sfx = TP.printf "%.2f" (x / 1_000) <> incSfx sfx

incSfx :: String -> String
incSfx "K" = "M"
incSfx "M" = "G"
incSfx "G" = "T"
incSfx x = x
