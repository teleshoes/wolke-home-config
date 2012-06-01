module Utils(
  height,
  fg, bg,
  img, circle, rect,
  title, clearSlave,
  clickArea,
  pos, posX, posY, lockX,
  posAbs, posAbsX, posAbsY, shiftUp, shiftMiddle,
  ignoreBG,
  padL, padR,
  chompAll, estimateLength,
  isRunning
) where
import System.Process(runCommand, system)
import System.Exit(ExitCode(ExitFailure))

-- CONSTANTS
height = 36

-- PRINTERS
fg color m = "^fg(" ++ color ++ ")" ++ m ++ "^fg()"
bg color m = "^bg(" ++ color ++ ")" ++ m ++ "^bg()"

img imgPath = "^i(" ++ imgPath ++ ")"
circle d = "^c(" ++ show d ++ ")"
rect x y = "^r(" ++ show x ++ "x" ++ show y ++ ")"

title m = "^tw()" ++ m ++ "\n"
clearSlave = "^cs()" ++ "\n"

clickArea _ "" m = m
clickArea btn cmd m = "^ca(" ++ show btn ++ ", " ++ cmd ++ ")" ++ m ++ "^ca()"

pos x y = "^p(" ++ show x ++ ";" ++ show y ++ ")"
posX x = "^p(" ++ show x ++ ")"
posY y = "^p(;" ++ show y ++ ")"
lockX m = "^p(_LOCK_X)" ++ m ++ "^p(_UNLOCK_X)"

posAbs x y = "^pa(" ++ show x ++ ";" ++ show y ++ ")"
posAbsX x = "^pa(" ++ show x ++ ")"
posAbsY y = "^pa(;" ++ show y ++ ")"
shiftUp = posAbsY 0
shiftMiddle = "^pa()"

ignoreBG m = "^ib(1)" ++ m ++ "^ib(0)"

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

