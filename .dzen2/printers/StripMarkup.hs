module StripMarkup(strip, estimateLength) where

--main = do putStr $ show $ estimateLength $ strip "start^i(PATH)mid^pa(;)end\n"

chomp ('\n':[]) = []
chomp (c:s) = c : chomp s
chomp [] = []

strip ('^':'^':s) = '^' : strip s
strip ('^':s) = strip $ skipTo ')' s
strip (c:s) = c : strip s
strip [] = []

skipTo ch (c:s) | ch == c = s
                | otherwise = skipTo ch s
skipTo _ [] = []

estimateLength = length . chomp . strip

