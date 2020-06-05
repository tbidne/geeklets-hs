module Network.SSID
  ( getSSID,
  )
where

import IOUtils

getSSID :: IO RunResultStr
getSSID =
  (fmap . fmap) ("SSID: " <>) $
    runCmd
      "/System/Library/PrivateFrameworks/Apple80211.framework/\
      \Versions/A/Resources/airport -I\
      \ | grep -i ssid | grep -iv BSSID\
      \ | awk \'{print $2}\'"
