module Utils(
  height,
  fg, bg,
  shiftUp, posAbs, posAbsX, posAbsY,
  estimateLength,
  isRunning
) where
import System.Process(runCommand, system)
import System.Exit(ExitCode(ExitFailure))

-- CONSTANTS
height = 36

-- PRINTERS
fg :: String -> String -> String
fg color markup = "^fg(" ++ color ++ ")" ++ markup ++ "^fg()"

bg :: String -> String -> String
bg color markup = "^bg(" ++ color ++ ")" ++ markup ++ "^bg()"

shiftUp = posAbsY 0

posAbs x y m = "^pa(" ++ show x ++ ";" ++ show y ++ ")" ++ m
posAbsX x m = "^pa(" ++ show x ++ ")" ++ m
posAbsY y m = "^pa(;" ++ show y ++ ")" ++ m

-- Parsing

estimateLength = length . chompAll . strip

chompAll = reverse . dropWhile (== '\n') . reverse

strip ('^':'^':s) = '^' : strip s
strip ('^':s) = strip $ dropWhile (/= ')') s
strip (c:s) = c : strip s
strip [] = []

-- IO
isRunning :: String -> IO Bool
isRunning p = do
  running <- system $ "pgrep " ++ p ++ " > /dev/null 2>/dev/null"
  return $ case running of
    ExitFailure _ -> False
    otherwise -> True

