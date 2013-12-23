module ProgressBar (progressBarW, getProgs, main) where
import JsonWidget (jsonWidgetNew)
import Color as C
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Utils (readInt, regexGroups, readProc, findName, chompFile)

main = do
  x <- getProgs
  print x

progressBarW = jsonWidgetNew $ fmap fmtProgs getProgs

fmtProgs :: [Integer] -> String
fmtProgs ps = "{" ++ (concat $ intersperse ", " (map fmtProg ps)) ++ "}"

fmtProg :: Integer -> String
fmtProg p = "\"label\": \"PRG\\n" ++ pct ++ "%\""
  where pct = if p == 100 then "@@" else show p

getProgs :: IO [Integer]
getProgs = do
  threadDelay $ 800 * 10^3
  files <- findName "/tmp" False "progress-bar*.txt"
  progs <- mapM chompFile files
  return $ catMaybes $ map readInt progs
