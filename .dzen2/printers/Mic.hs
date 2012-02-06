module Mic(main) where
import System.Environment (getEnv)
import System.Process(readProcess)
import ClickAction (clickAction)
import ClickableImage (clickableImage)

import Volume (isMuted)

height = 36
diameter = height `div` 4

main = do
 muted <- isMuted "microphone"
 putStr $ formatCircle muted

formatCircle isM = clickAction "1" clickCmd markup
  where markup = color $ "^p(;" ++ (show shift) ++ ")^c(" ++ (show d) ++ ")"
        d = diameter
        shift = height `div` 2 - d `div` 2
        color m = "^fg(" ++ fg ++ ")" ++ m ++ "^fg()"
        fg = if isM then "black" else "red"

clickCmd = "pulse-vol microphone toggle"
