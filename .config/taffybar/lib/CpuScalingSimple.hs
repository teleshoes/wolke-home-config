module CpuScalingSimple(cpuScalingSimpleW) where
import Utils (fg, bg, readInt, readProc)
import Label (labelW, mainLabel)

main = mainLabel cpuScalingSimpleReader
cpuScalingSimpleW = labelW cpuScalingSimpleReader

cpuScalingSimpleReader = do
  pct <- get
  return $ format pct

get = fmap toPct $ readProc ["sudo", "cpu-set", "-g"]

format :: Integer -> String
format pct = color $ fmt pct
  where color | pct < 0 || pct > 100 = bg "white" . fg "black"
              | pct < 25 = bg "red" . fg "black"
              | pct < 50 = bg "orange" . fg "black"
              | pct < 100 = bg "green" . fg "black"
              | pct == 100 = bg "blue"
        fmt x | x < 0 || x > 100 = "??"
              | x < 10 = '0':show x
              | x == 100 = "HH"
              | otherwise = show x

toPct :: String -> Integer
toPct n = case readInt n of
                 Just pct | (0 <= pct && pct <= 100) -> pct
                 _ -> 0-1
