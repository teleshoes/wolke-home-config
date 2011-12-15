module Columns(main, columns, buckets) where
import System (getArgs)
import System.IO (hIsTerminalDevice, hIsOpen, hGetContents, stdin)
import Data.List (intercalate, intersperse, transpose)
import Data.List.Split (splitEvery, splitPlaces)
import StripMarkup (estimateLength)

main = do
  ensureNotTerminal
  args <- getArgs
  if length args /= 1 then error "Usage: <column-count>\n" else return ()
  lns <- fmap lines getAllStdin
  let col = (read :: String -> Int) (args !! 0)
  putStr $ columns (buckets col lns) "==="

buckets :: Int -> [a] -> [[a]]
buckets cnt xs = splitPlaces (getPlaces cnt (length xs)) xs

getPlaces _ 0 = []
getPlaces p _ | p <= 0 = error "split non-empty list into 0 or less pieces\n"
getPlaces p n = place : getPlaces (p-1) (n-place)
  where place = n`div`p + if n`mod`p > 0 then 1 else 0

ensureNotTerminal = do
  term <- hIsTerminalDevice stdin
  let msg = ""
            ++ "Must read entire stdinput first; "
            ++ "dont connect me to a terminal"
  if term then error msg else return ()

getAllStdin = do
  open <- hIsOpen stdin
  if not open then return "" else do
    contents <- hGetContents stdin
    moreContents <- getAllStdin
    return $ contents ++ moreContents

pad len s = s ++ replicate (len-length s) ' '

columns :: [[String]] -> String -> String
columns cols sep = intercalate "\n" $ map concat $ map (intersperse sep) rows
  where
    paddedCols = zipWith (\len col -> map (pad len) col) maxLens cols
    maxLens = map (maximum . (map estimateLength)) cols
    rows = transpose paddedCols

