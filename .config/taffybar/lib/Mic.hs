module Mic(micW) where
import Clickable (clickableLeft)
import Label (labelW, mainLabel)
import Utils (fg)
import Volume (isMuted)

import System.Environment (getEnv)

main = mainLabel micReader
micW = clickableLeft clickCmd =<< labelW micReader

micReader = do
  muted <- isMuted "microphone"
  return $ fg (if muted then "black" else "red") "M"

clickCmd = "pulse-vol microphone toggle"
