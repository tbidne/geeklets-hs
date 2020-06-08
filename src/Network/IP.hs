{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Network.IP
  ( getIPs,
  )
where

import IOUtils

data IPType
  = Local
  | Global

newtype IP (a :: IPType) = MkIP String

getIPs :: IO RunResultStr
getIPs = do
  l <- ipLocal
  g <- ipGlobal
  pure $ combineIPs <$> l <*> g

combineIPs :: IP 'Local -> IP 'Global -> String
combineIPs l g =
  dispLocalIP l
    <> "\n"
    <> dispGlobalIP g

ipLocal :: IO (RunResult (IP 'Local))
ipLocal =
  (fmap . fmap) MkIP $
    runCmd
      "ifconfig en0\
      \ | grep inet | grep -v inet6\
      \ | awk '{print $2}'"

ipGlobal :: IO (RunResult (IP 'Global))
ipGlobal = (fmap . fmap) MkIP $ runCmd "curl --silent http://ipecho.net/plain"

dispLocalIP :: IP 'Local -> String
dispLocalIP (MkIP s) = "Local  IP: " <> s

dispGlobalIP :: IP 'Global -> String
dispGlobalIP (MkIP s) = "Global IP: " <> s