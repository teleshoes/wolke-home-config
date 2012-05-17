module Utils(isRunning) where
import System.Process(runCommand, system)
import System.Exit(ExitCode(ExitFailure))

isRunning :: String -> IO Bool
isRunning p = do
  running <- system $ "pidof " ++ p ++ " > /dev/null 2>/dev/null"
  return $ case running of
    ExitFailure _ -> False
    otherwise -> True

