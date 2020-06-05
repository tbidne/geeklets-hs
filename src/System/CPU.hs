module System.CPU
  ( getCPU1,
    getCPU2,
  )
where

import Common
import IOUtils

getCPU1 :: IO RunResultStr
getCPU1 = cpu1IO >>= (pure . (=<<) intStrToGeekLetsChar)

getCPU2 :: IO RunResultStr
getCPU2 = cpu2IO >>= (pure . (=<<) intStrToGeekLetsChar)

cpu1IO :: IO RunResultStr
cpu1IO =
  runCmd
    "top -l2 \
    \ | grep \"CPU usage\"\
    \ | head -1\
    \ | awk '{print int(($3+$5)/2)}\'"

cpu2IO :: IO RunResultStr
cpu2IO =
  runCmd
    "top -l2 \
    \ | grep \"CPU usage\"\
    \ | tail -1\
    \ | awk '{print int(($3+$5)/2)}\'"
