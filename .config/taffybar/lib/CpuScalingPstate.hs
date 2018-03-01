module CpuScalingPstate(cpuScalingPstateW) where
import Utils (
  fg, bg, padL, regexGroups,
  readInt, collectInts, chompFile, readProc)
import Label (labelW, mainLabel)

import Control.Monad (void)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Control.Concurrent (forkIO)
import Control.Error (note)
import System.Process (system)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)

main = mainLabel cpuScalingPstateReader
cpuScalingPstateW = labelW cpuScalingPstateReader

cpuScalingPstateReader = do
  min <- get "min"
  max <- get "max"
  return $ format min max

get dev = fmap toPct $ readProc ["sudo", "intel-pstate", "-g", dev]

format :: Integer -> Integer -> String
format min max = color $ fmt max ++ "\n" ++ fmt min
  where color | max < 0 || max > 100 = bg "white" . fg "black"
              | max < 33 = bg "red" . fg "black"
              | max < 66 = bg "orange" . fg "black"
              | max < 100 = bg "green" . fg "black"
              | max == 100 = bg "blue"
        fmt x | x < 0 || x > 100 = "??"
              | x < 10 = '0':show x
              | x == 100 = "HH"
              | otherwise = show x

toPct :: String -> Integer
toPct n = case readInt n of
                 Just pct | (0 <= pct && pct <= 100) -> pct
                 _ -> 0-1
