import System.Process(readProcess)

main = do
  y <- readProcess "date" ["+%Y"] ""
  m <- readProcess "date" ["+%m"] ""
  d <- readProcess "date" ["+%d"] ""
  prev <- cal $ prevMonth (i y, i m)
  this <- cal $ thisMonth (i y, i m)
  next <- cal $ nextMonth (i y, i m)
  putStr (take 1 $ lines prev)
--  putStr $ prev ++ this ++ next

i = read :: String -> Integer

chomp ('\n':[]) = []
chomp (c:s) = c : chomp s
chomp [] = []

thisMonth (y,m)             = (y, m)
prevMonth (y,m) | m == 0    = (y-1, 12)
                | otherwise = (y,  m-1)
nextMonth (y,m) | m == 12   = (y+1,  0)
                | otherwise = (y,  m+1)

cal (y,m) = readProcess "cal" ["-h", show m, show y] ""


--styleToday date cal = replace date styledDate


styledDate d = ""
  ++ "^bg(grey70)"
  ++ "^fg(#111111)"
  ++ d
  ++ "^fg()"
  ++ "^bg()"

stripZero ('0':s) = ' ':s
stripZero s = s

-- $today =~ s/^0/ /;
-- $thisMonthDisplay =~ s/(\s)$today(\s)/$1$styledToday$2/sx;


