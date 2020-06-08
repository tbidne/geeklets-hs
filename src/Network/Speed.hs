{-# LANGUAGE DataKinds #-}
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

import Bytes

getSpeed :: IO RunResultStr
getSpeed = do
  b1 <- speedIO
  sleep1Sec
  b2 <- speedIO
  let p1 = b1 >>= parseRaw
  let p2 = b2 >>= parseRaw
  let diff = liftA2 (-) p1 p2
  pure $ fmap (dispBytes formatDown formatUp) diff

sleep1Sec :: IO ()
sleep1Sec = C.threadDelay 1_000_000 -- microseconds

speedIO :: IO RunResultStr
speedIO =
  runCmd
    "netstat -bI \'en0\'\
    \ | head -2\
    \ | awk \"/en0/\"\'{print $7, $10}\'"

parseRaw :: String -> RunResult (BytesPair Integer)
parseRaw s =
  let (_, _, _, matches) = s `capture` regex
   in case matches of
        [downStr, upStr] -> readBytesPair downStr upStr
        _ -> RFailure $ "Incorrectly formatted: " <> show matches

regex :: String
regex = "([0-9]+) ([0-9]+)"

formatDown :: Bytes 'Down Integer -> String
formatDown (MkBytes x) = "Down: " <> formatInteger x

formatUp :: Bytes 'Up Integer -> String
formatUp (MkBytes x) = "Up: " <> formatInteger x

formatInteger :: Integer -> String
formatInteger i
  | i < 1000 = TP.printf "%.1f KB/s" $ fromIntegral @Integer @Float i
  | otherwise = TP.printf "%.1f MB/s" $ fromIntegral @Integer @Float i / 1_00
