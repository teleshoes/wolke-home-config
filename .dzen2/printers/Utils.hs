module Utils(fg, bg, isRunning) where
import System.Process(runCommand, system)
import System.Exit(ExitCode(ExitFailure))

fg :: String -> String -> String
fg color markup = "^fg(" ++ color ++ ")" ++ markup ++ "^fg()"

bg :: String -> String -> String
bg color markup = "^bg(" ++ color ++ ")" ++ markup ++ "^bg()"

isRunning :: String -> IO Bool
isRunning p = do
  running <- system $ "pgrep " ++ p ++ " > /dev/null 2>/dev/null"
  return $ case running of
    ExitFailure _ -> False
    otherwise -> True

