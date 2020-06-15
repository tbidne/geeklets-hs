{-# LANGUAGE DataKinds #-}

module Network.IP
  ( getIPs,
  )
where

import IOUtils
import Network.IP.Internal

getIPs :: IO RunResultStr
getIPs = do
  l <- ipLocal
  g <- ipGlobal
  pure $ combineIPs <$> l <*> g
ipLocal :: IO (RunResult (IP 'Local))
ipLocal =
  (fmap . fmap) MkIP $
    runCmd
      "ifconfig en0\
      \ | grep inet | grep -v inet6\
      \ | awk '{print $2}'"

ipGlobal :: IO (RunResult (IP 'Global))
ipGlobal = (fmap . fmap) MkIP $ runCmd "curl --silent http://ipecho.net/plain"