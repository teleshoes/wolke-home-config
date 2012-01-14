module Brightness (main) where
import System.Environment (getEnv)
import System.Process(readProcess)
import PercentBar (percentBar)

main = do
  brightness <- fmap (round.d) $ readProcess "xbacklight" ["-get"] ""
  let colors = ["black", "darkgray"] ++ repeat "orange"
  putStr $ percentBar brightness colors 5 3

d = read :: String -> Double

