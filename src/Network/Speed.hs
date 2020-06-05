{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Network.Speed
  ( getSpeed,
  )
where

import Common
import Control.Applicative
import qualified Control.Concurrent as C
import IOUtils
import qualified Text.Printf as TP

newtype Up = MkUp Integer deriving (Show)

newtype Down = MkDown Integer deriving (Show)

newtype Speed = MkSpeed (Down, Up) deriving (Show)

getSpeed :: IO RunResultStr
getSpeed = do
  s1 <- speedIO
  sleep1Sec
  s2 <- speedIO
  let p1 = s1 >>= parseRaw
  let p2 = s2 >>= parseRaw
  let diff = liftA2 diffSpeed p1 p2
  pure $ fmap dispSpeed diff

sleep1Sec :: IO ()
sleep1Sec = C.threadDelay 1_000_000 -- microseconds

speedIO :: IO RunResultStr
speedIO =
  runCmd
    "netstat -bI \'en0\'\
    \ | head -2\
    \ | awk \"/en0/\"\'{print $7, $10}\'"

parseRaw :: String -> RunResult Speed
parseRaw s =
  let (_, _, _, matches) = s `capture` regex
   in case matches of
        [downStr, upStr] ->
          let down = readInteger downStr
              up = readInteger upStr
           in liftA2 (\d u -> MkSpeed (MkDown d, MkUp u)) down up
        _ -> RFailure $ "Incorrectly formatted: " <> show matches

regex :: String
regex = "([0-9]+) ([0-9]+)"

dispSpeed :: Speed -> String
dispSpeed (MkSpeed (MkDown d, MkUp u)) = "Down: " <> f d <> "\nUp:     " <> f u
  where
    f i
      | i < 1000 = TP.printf "%.1f KB/s" $ fromIntegral @Integer @Float i
      | otherwise = TP.printf "%.1f MB/s" $ fromIntegral @Integer @Float i / 1_000

diffSpeed :: Speed -> Speed -> Speed
diffSpeed (MkSpeed (MkDown d1, MkUp u1)) (MkSpeed (MkDown d2, MkUp u2)) =
  MkSpeed (MkDown (d2 - d1), MkUp (u2 - u1))
