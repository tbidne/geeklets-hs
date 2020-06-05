module IOUtils
  ( runCmd,
    RunResult (..),
    RunResultStr,
  )
where

import Common
import qualified System.Exit as Exit
import qualified System.Process as P
import Types

-- treat empty output as error
runCmd :: String -> IO RunResultStr
runCmd cmd = do
  (code, out, err) <- shExitCode cmd
  pure $ case code of
    Exit.ExitSuccess
      | out /= "" -> RSuccess $ trim out
      | otherwise -> RFailure "no output"
    Exit.ExitFailure _ -> RFailure err

shExitCode :: String -> IO (Exit.ExitCode, String, String)
shExitCode cmd = P.readCreateProcessWithExitCode (P.shell cmd) ""
