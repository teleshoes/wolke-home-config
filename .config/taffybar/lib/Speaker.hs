module Speaker(speakerW) where
import Clickable (clickableRight)
import Label (labelW)
import Utils (fg, readProc, regexMatch)

import System.Environment (getEnv)

speakerW = clickableRight clickCmd =<< labelW getSpeakerMarkup

getSpeakerMarkup = fmap format getDefaultSink

getDefaultSink :: IO String
getDefaultSink = readProc ["speaker", "--default"]

format s | regexMatch "hdmi" s        = fg "white" "hdmi"
format s | regexMatch "pci.*analog" s = fg "blue" "pci"
format s | regexMatch "usb" s         = fg "yellow" "usb"
format _                              = fg "red" "unknown"

clickCmd = "speaker --cycle"
