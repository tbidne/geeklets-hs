module System.Uptime
  ( getUptime,
  )
where

import IOUtils

getUptime :: IO RunResultStr
getUptime =
  runCmd
    "uptime | \
    \cut -c 11-100 | \
    \awk \'{split($0, a, \"[ mins]*, [1234567890]+ user\"); \
    \sub(\":\", \"h \", a[1]); sub(\" day,  \", \"d \", a[1]); \
    \print \"Up for \" a[1] \" mins\" }\'"
