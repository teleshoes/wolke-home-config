module Utils(
  height,
  fg, bg,
  img, circle, rect,
  title, clearSlave,
  clickArea,
  pos, posX, posY, lockX,
  posAbs, posAbsX, posAbsY, shiftTop, shiftMid, shiftBot,
  ignoreBG,
  regexMatch, regexGroups, regexFirstGroup,
  readInt, padL, padR, chompAll, estimateLength,
  lineBuffering, isRunning, chompFile, systemReadLines, readProc, procSuccess
) where
import System.Exit(ExitCode(ExitFailure), ExitCode(ExitSuccess))
import System.Directory (doesFileExist)
import Text.Regex.PCRE ((=~))
import Data.Maybe (fromMaybe, listToMaybe)
import System.IO (
  BufferMode(LineBuffering), stdout, hSetBuffering, hGetContents)
import System.Process (
  StdStream(CreatePipe), std_out, createProcess, shell,
  readProcessWithExitCode, system)

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
shiftTop = posAbsY 0
shiftMid = "^pa()"
shiftBot = posAbsY $ height `div` 2

ignoreBG m = "^ib(1)" ++ m ++ "^ib(0)"

-- Parsing
regexMatch :: String -> String -> Bool
regexMatch = flip (=~)
regexGroups :: String -> String -> Maybe [String]
regexGroups re str = fmap (drop 1) $ listToMaybe $ str =~ re
regexFirstGroup :: String -> String -> Maybe String
regexFirstGroup re str = listToMaybe $ fromMaybe [] $ regexGroups re str

readInt :: String -> Maybe Integer
readInt s = case reads s of
              ((x,_):_) -> Just x
              _ -> Nothing

padL x len xs = replicate (len - length xs) x ++ xs
padR x len xs = xs ++ replicate (len - length xs) x

chompAll = reverse . dropWhile (== '\n') . reverse

estimateLength = length . chompAll . stripDzenMarkup

stripDzenMarkup ('^':'^':s) = '^' : stripDzenMarkup s
stripDzenMarkup ('^':s) = stripDzenMarkup $ drop 1 $ dropWhile (/= ')') s
stripDzenMarkup (c:s) = c : stripDzenMarkup s
stripDzenMarkup [] = []

-- IO
lineBuffering = hSetBuffering stdout LineBuffering

isRunning :: String -> IO Bool
isRunning p = do
  running <- system $ "pgrep " ++ p ++ " > /dev/null 2>/dev/null"
  return $ case running of
    ExitFailure _ -> False
    otherwise -> True

chompFile file = do
  curExists <- doesFileExist file
  if curExists then fmap chompAll $ readFile file else return ""

systemReadLines :: String -> IO [String]
systemReadLines cmd = fmap lines $ sys >>= \(_,Just h,_,_) -> lineBufContent h
  where sys = createProcess (shell cmd) {std_out = CreatePipe}
        lineBufContent h = hSetBuffering h LineBuffering >> hGetContents h

readProc (cmd:args) = fmap snd3 $ readProcessWithExitCode cmd args ""
  where snd3 (_,x,_) = x

procSuccess (cmd:args) = do
  (exitCode,_,_) <- readProcessWithExitCode cmd args ""
  return $ exitCode == ExitSuccess

listToChan :: [a] -> IO (Chan a)
listToChan xs = newChan >>= (\c -> forkIO (writeList2Chan c xs) >> return c)
