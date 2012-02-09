module Brightness (main) where
import System.Environment (getEnv)
import System.Process(readProcess, system)
import PercentBar (percentBar)
import Control.Concurrent (threadDelay)

main = do
  threadDelay $ 1*10^6
  system "$HOME/bin/brightness last > /dev/null"
  brightness <- fmap (round.d) $ readProcess "xbacklight" ["-get"] ""
  let colors = ["black", "darkgray"] ++ repeat "orange"
  putStr $ percentBar brightness colors 5 3

d = read :: String -> Double

