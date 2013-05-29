module Mic(micW) where
import Clickable (clickableLeft)
import Label (labelW)
import Utils (fg)
import Volume (isMuted)

import System.Environment (getEnv)

micW = clickableLeft clickCmd =<< labelW getMic

getMic = do
  muted <- isMuted "microphone"
  return $ fg (if muted then "black" else "red") "M"

clickCmd = "pulse-vol microphone toggle"
