module Network.IP
  ( getIPs,
  )
where

import IOUtils

getIPs :: IO RunResultStr
getIPs = do
  l <- ipLocal
  g <- ipGlobal
  let ips = (,) <$> l <*> g
  pure $ fmap combineIPs ips

combineIPs :: (String, String) -> String
combineIPs (l, g) =
  "Local  IP: "
    <> l
    <> "\nGlobal IP: "
    <> g

ipLocal :: IO RunResultStr
ipLocal =
  runCmd
    "ifconfig en0\
    \ | grep inet | grep -v inet6\
    \ | awk '{print $2}'"

ipGlobal :: IO RunResultStr
ipGlobal = runCmd "curl --silent http://ipecho.net/plain"
