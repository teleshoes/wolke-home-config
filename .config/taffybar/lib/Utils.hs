module Utils(
  height,
  fg, bg, fgbg,
  regexMatch, regexAllMatches, regexGroups, regexFirstGroup,
  readInt, readDouble, collectInts, padL, padR, chompAll, estimateLength,
  nanoTime, lineBuffering, isRunning, chompFile,
  systemReadLines, readProc, chompProc, procSuccess,
  actToChanDelay, listToChan
) where
import Control.Concurrent (
  forkIO, threadDelay,
  Chan, writeChan, writeList2Chan, newChan)
import Control.Monad (forever, void)
import System.Exit(ExitCode(ExitFailure), ExitCode(ExitSuccess))
import System.Directory (doesFileExist)
import Text.Regex.PCRE ((=~))
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import System.IO (
  BufferMode(LineBuffering), stdout, hSetBuffering, hGetContents)
import System.Process (
  StdStream(CreatePipe), std_out, createProcess, shell,
  readProcessWithExitCode, system)
import System.Posix.Clock (timeSpecToInt64, monotonicClock, getClockTime)

-- CONSTANTS
height = 36

-- MARKUP
fg color m = "<span foreground=\"" ++ color ++ "\">" ++ m ++ "</span>"
bg color m = "<span background=\"" ++ color ++ "\">" ++ m ++ "</span>"
fgbg fg bg m = "<span"
               ++ " foreground=\"" ++ fg ++ "\""
               ++ " background=\"" ++ bg ++ "\""
               ++ ">" ++ m ++ "</span>"

-- Parsing
regexMatch :: String -> String -> Bool
regexMatch = flip (=~)
regexGroups :: String -> String -> Maybe [String]
regexGroups re str = fmap (drop 1) $ listToMaybe $ str =~ re
regexFirstGroup :: String -> String -> Maybe String
regexFirstGroup re str = listToMaybe $ fromMaybe [] $ regexGroups re str
regexAllMatches :: String -> String -> [String]
regexAllMatches re str = concatMap (take 1) $ str =~ re

readInt :: String -> Maybe Integer
readInt s = case reads s of
              ((x,_):_) -> Just x
              _ -> Nothing

readDouble :: String -> Maybe Double
readDouble s = case reads s of
              ((x,_):_) -> Just x
              _ -> Nothing

collectInts :: String -> [Integer]
collectInts = catMaybes . (map readInt) . (regexAllMatches "\\d+")

padL x len xs = replicate (len - length xs) x ++ xs
padR x len xs = xs ++ replicate (len - length xs) x

chompAll = reverse . dropWhile (== '\n') . reverse

estimateLength = length . chompAll . stripDzenMarkup

stripDzenMarkup ('^':'^':s) = '^' : stripDzenMarkup s
stripDzenMarkup ('^':s) = stripDzenMarkup $ drop 1 $ dropWhile (/= ')') s
stripDzenMarkup (c:s) = c : stripDzenMarkup s
stripDzenMarkup [] = []

-- IO
nanoTime :: IO Integer
nanoTime = fmap (fromIntegral . timeSpecToInt64) $ getClockTime monotonicClock

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

chompProc = fmap chompAll . readProc

procSuccess (cmd:args) = do
  (exitCode,_,_) <- readProcessWithExitCode cmd args ""
  return $ exitCode == ExitSuccess

actToChanDelay :: Int -> IO a -> IO (Chan a)
actToChanDelay delayMicro act = do
  chan <- newChan
  forkIO $ forever $ do
    start <- nanoTime
    act >>= writeChan chan
    end <- nanoTime
    let elapsedMicro = (fromIntegral $ end - start) `div` 10^3
    threadDelay $ delayMicro - elapsedMicro
  return chan

listToChan :: [a] -> IO (Chan a)
listToChan xs = newChan >>= (\c -> forkIO (writeList2Chan c xs) >> return c)
