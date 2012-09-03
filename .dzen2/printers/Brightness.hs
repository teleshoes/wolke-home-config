module Brightness (main) where
import System.Environment (getEnv)
import System.Process(readProcess, system)
import PercentBar (percentBar)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))
import Utils (readDouble, readProc)

main = do
  threadDelay $ 1*10^6
  system "$HOME/bin/brightness last > /dev/null"
  brightness <- parse <$> readProc ["xbacklight", "-get"]
  let colors = ["black", "darkgray"] ++ repeat "orange"
  putStr $ percentBar brightness colors 5

parse = round . fromMaybe 300.0 . readDouble

