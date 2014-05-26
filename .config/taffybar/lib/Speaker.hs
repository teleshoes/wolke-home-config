module Speaker(speakerW) where
import Clickable (clickableRight)
import Label (labelW)
import Utils (padL, fg, readProc, regexMatch)

import System.Environment (getEnv)

speakerW = clickableRight clickCmd =<< labelW getSpeakerMarkup

getSpeakerMarkup = fmap format getDefaultSink

getDefaultSink :: IO String
getDefaultSink = readProc ["speaker", "--default"]

format s | regexMatch "hdmi" s        = pad "hdmi"
format s | regexMatch "pci.*analog" s = fg "green" $ pad "pci"
format s | regexMatch "usb" s         = fg "yellow" $ pad "usb"
format _                              = fg "red" $ pad "unknown"

pad = padL ' ' 4

clickCmd = "speaker --cycle"
