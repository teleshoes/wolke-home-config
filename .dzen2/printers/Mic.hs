module Mic(main) where
import System.Environment (getEnv)
import System.Process(readProcess)
import ClickAction (clickAction)
import ClickableImage (clickableImage)

import Volume (isMuted)
import Utils (height, fg, circle, posY)

diameter = height `div` 4

main = do
 muted <- isMuted "microphone"
 putStr $ formatCircle muted

formatCircle isM = clickAction 1 clickCmd markup
  where markup = fg color $ posY shift $ circle diameter
        shift = 3 * diameter `div` 2
        color = if isM then "black" else "red"

clickCmd = "pulse-vol microphone toggle"
