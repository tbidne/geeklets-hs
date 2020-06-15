{-# LANGUAGE NumericUnderscores #-}

module Network.Speed
  ( getSpeed,
  )
where

--import Bytes
import Control.Applicative (liftA2)
import Control.Monad (join)
import qualified Control.Concurrent as C
import IOUtils
import Network.Speed.Internal

getSpeed :: IO RunResultStr
getSpeed = do
  b1 <- speedIO
  sleep1Sec
  b2 <- speedIO
  pure $ join $ liftA2 parseAndDisplayDiff b1 b2

sleep1Sec :: IO ()
sleep1Sec = C.threadDelay 1_000_000 -- microseconds

speedIO :: IO RunResultStr
speedIO =
  runCmd
    "netstat -bI \'en0\'\
    \ | head -2\
    \ | awk \"/en0/\"\'{print $7, $10}\'"