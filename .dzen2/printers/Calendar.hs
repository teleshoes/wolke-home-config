module Calendar(main) where
import System.Process(readProcess)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

main = do
  y <- dateIntField "Y" 
  m <- dateIntField "m" 
  d <- dateIntField "d" 
  cals <- getCals y m d [-1, 0, 1, 2, 3, 4]
  putStr $ formatTall $ cals

dateIntField f = (read :: String -> Int) <$> readProcess "date" ["+%" ++ f] ""

cal (y,m) = readProcess "cal" ["-h", show m, show y] ""

getCals y m d (n:ns) = do
  c <- cal $ relMonth (y,m) n
  cs <- getCals y m d ns
  return ((if n == 0 then styleThisMonth d c else c):cs)
getCals _ _ _ [] = return []

styleThisMonth d c | length lns == 0 = c
                   | otherwise = unlines $ (highlight $ head lns):tail lns
  where lns = lines $ styleDate (show d) c
        highlight ln = "^bg(blue)" ++ ln ++ "^bg()"

formatTall xs = concat xs
formatWide xs = collate (map lines xs) "\n"
collate [] _ = []
collate xs sep = heads ++ sep ++ collate tails sep
  where fxs = filter (/= []) xs
        heads = concatMap head fxs
        tails = map tail fxs

relMonth (y,m) n | n == 0 = (y,m)
                 | n > 0 = relMonth (nextMonth (y,m)) (n-1)
                 | n < 0 = relMonth (prevMonth (y,m)) (n+1)
prevMonth (y,m) | m == 1    = (y-1, 12)
                | otherwise = (y,  m-1)
nextMonth (y,m) | m == 12   = (y+1,  1)
                | otherwise = (y,  m+1)

styleDate date cal = unlines (map rep $ lines cal)
  where rep = replaceDate (date) (wrapStyle (spacePrepend 2 date) ++ " ")

spacePrepend width s | length s >= width = s
                     | otherwise = spacePrepend width (' ':s)

replaceDate d sD (x:y:' ':xs)   | (x:y:"") == d = sD ++ replaceDate d sD xs
replaceDate d sD (x:y:[])       | (x:y:"") == d = sD
replaceDate d sD (' ':y:' ':xs) | (y:"") == d = sD ++ replaceDate d sD xs
replaceDate d sD (' ':y:[])     | (y:"") == d = sD
replaceDate d sD (x:y:z:xs) = x:y:z:(replaceDate d sD xs)
replaceDate _ _ xs = xs

wrapStyle d = ""
  ++ "^bg(grey70)"
  ++ "^fg(#111111)"
  ++ d
  ++ "^fg()"
  ++ "^bg()"

