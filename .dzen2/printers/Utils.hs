module Utils(height, fg, bg, posAbs, posAbsX, posAbsY, isRunning) where
import System.Process(runCommand, system)
import System.Exit(ExitCode(ExitFailure))

-- CONSTANTS
height = 36

-- PRINTERS
fg :: String -> String -> String
fg color markup = "^fg(" ++ color ++ ")" ++ markup ++ "^fg()"

bg :: String -> String -> String
bg color markup = "^bg(" ++ color ++ ")" ++ markup ++ "^bg()"

posAbs x y m = "^pa(" ++ show x ++ ";" ++ show y ++ ")" ++ m
posAbsX x m = "^pa(" ++ show x ++ ")" ++ m
posAbsY y m = "^pa(;" ++ show y ++ ")" ++ m

-- IO
isRunning :: String -> IO Bool
isRunning p = do
  running <- system $ "pgrep " ++ p ++ " > /dev/null 2>/dev/null"
  return $ case running of
    ExitFailure _ -> False
    otherwise -> True
