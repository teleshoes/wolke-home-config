module Utils(
  height,
  fg, bg,
  shiftUp, posAbs, posAbsX, posAbsY,
  padL, padR,
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

padL x len xs = replicate (len - length xs) x ++ xs
padR x len xs = xs ++ replicate (len - length xs) x

estimateLength = length . chompAll . stripDzenMarkup

chompAll = reverse . dropWhile (== '\n') . reverse

stripDzenMarkup ('^':'^':s) = '^' : stripDzenMarkup s
stripDzenMarkup ('^':s) = stripDzenMarkup $ drop 1 $ dropWhile (/= ')') s
stripDzenMarkup (c:s) = c : stripDzenMarkup s
stripDzenMarkup [] = []

-- IO
isRunning :: String -> IO Bool
isRunning p = do
  running <- system $ "pgrep " ++ p ++ " > /dev/null 2>/dev/null"
  return $ case running of
    ExitFailure _ -> False
    otherwise -> True

