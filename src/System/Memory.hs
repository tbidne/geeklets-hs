module System.Memory
  ( getMemory,
  )
where

import Common
import Control.Monad ((>=>))
import IOUtils

getMemory :: IO RunResultStr
getMemory = memoryIO >>= (pure . (=<<) parseRaw)

memoryIO :: IO RunResultStr
memoryIO =
  runCmd
    "memory_pressure\
    \ | grep \"System-wide memory free percentage\"\
    \ | awk \'{print int($5)/2}\'"

parseRaw :: String -> RunResult String
parseRaw = matchRx >=> intStrToGeekLetsChar

matchRx :: String -> RunResultStr
matchRx s =
  case captureOne s regex of
    "" -> RFailure $ "Could not parse number from " <> s
    s' -> RSuccess s'

regex :: String
regex = "([0-9]+)"
