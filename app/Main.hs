{-# LANGUAGE GADTs #-}

module Main
  ( main,
  )
where

import qualified System.Environment as SysEnv
import qualified Network
import qualified System
import Types
import Freer

main :: IO ()
main = foldFreer interpretIO runCLI

interpretIO :: CLI a -> IO a
interpretIO GetArgs = SysEnv.getArgs
interpretIO (ParseArgs xs) = pure $ parseOneArg xs
interpretIO (RunCmd cmd) = runCmdIO cmd
interpretIO (PutStrLn s) = putStrLn s

parseOneArg :: [String] -> ParseResult
parseOneArg [] = PFailure "No args given"
parseOneArg [x] = PSuccess x
parseOneArg _ = PFailure "Expected 1 arg, received too many"

runCmdIO :: String -> IO RunResultStr
runCmdIO "ip" = Network.getIPs
runCmdIO "speed" = Network.getSpeed
runCmdIO "ssid" = Network.getSSID
runCmdIO "total" = Network.getTotal
runCmdIO "cpu1" = System.getCPU1
runCmdIO "cpu2" = System.getCPU2
runCmdIO "memory" = System.getMemory
runCmdIO "uptime" = System.getUptime
runCmdIO x = pure $ RFailure $ "Unrecognized arg: " <> show x