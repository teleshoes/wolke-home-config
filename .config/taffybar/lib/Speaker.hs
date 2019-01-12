module Speaker(speakerW) where
import Clickable (clickableRight)
import Label (labelW, mainLabel)
import Utils (padL, fg, readProc, regexMatch)

import System.Environment (getEnv)

main = mainLabel speakerReader
speakerW = clickableRight clickCmd =<< labelW speakerReader

speakerReader = fmap format getDefaultSink

getDefaultSink :: IO String
getDefaultSink = readProc ["speaker", "--default"]

format s | regexMatch "hdmi" s        = pad "hdmi"
format s | regexMatch "pch" s         = fg "green" $ pad "pch"
format s | regexMatch "usb" s         = fg "yellow" $ pad "usb"
format _                              = fg "red" $ pad "unknown"

pad = padL ' ' 4

clickCmd = "speaker --cycle"
