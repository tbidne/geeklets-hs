module Common
  ( capture,
    captureAll,
    captureOne,
    intToChar,
    intStrToGeekLetsChar,
    readInt,
    readInteger,
    trim,
  )
where

import Control.Monad ((>=>))
import qualified Text.Read as TR
import qualified Text.Regex.TDFA as Rx
import Types

capture :: String -> String -> (String, String, String, [String])
capture = (Rx.=~)

captureOne :: String -> String -> String
captureOne = (Rx.=~)

captureAll :: String -> String -> [String]
captureAll s rx = Rx.getAllTextMatches (s Rx.=~ rx)

-- GeekTool interprets a single alpha letter char as the amount of an arc length
-- to draw. The range is: [ 'a', ... , 'z', 'A', ... , 'Y' ]. This is 51 letters,
-- so valid indices are in [0 - 50].
intToChar :: Int -> RunResult String
intToChar i
  | i >= 0 && i <= 50 = RSuccess [chars !! i]
  | otherwise = RFailure $ "Index out of range (0-50): " <> show i

chars :: String
chars = ['a' .. 'z'] ++ ['A' .. 'Y']

readInt :: String -> RunResult Int
readInt s =
  case TR.readMaybe s of
    Just i -> RSuccess i
    Nothing -> RFailure $ "Could not parse to int: " <> s

readInteger :: String -> RunResult Integer
readInteger s =
  case TR.readMaybe s of
    Just f -> RSuccess f
    Nothing -> RFailure $ "Could not convert to integer: " <> s

intStrToGeekLetsChar :: String -> RunResult String
intStrToGeekLetsChar = readInt >=> intToChar

trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' = dropWhile (\c -> c == ' ' || c == '\n')
