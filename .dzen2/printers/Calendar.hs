import System.Process(readProcess)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (fromMaybe)

main = do
  y <- readProcess "date" ["+%Y"] ""
  m <- readProcess "date" ["+%m"] ""
  d <- readProcess "date" ["+%d"] ""
  prev <- cal $ relMonth (i y, i m) (0-1)
  this <- cal $ relMonth (i y, i m) (0+0)
  next <- cal $ relMonth (i y, i m) (0+1)
  putStr $ formatTall [prev, (styleDate (chomp d) this), next]

i = read :: String -> Integer

chomp s = reverse $ fromMaybe (reverse s) $ stripPrefix "\n" $ reverse s

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
prevMonth (y,m) | m == 0    = (y-1, 12)
                | otherwise = (y,  m-1)
nextMonth (y,m) | m == 12   = (y+1,  0)
                | otherwise = (y,  m+1)

cal (y,m) = readProcess "cal" ["-h", show m, show y] ""


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

