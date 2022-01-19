module Mic(micW) where
import Clickable (clickableLeft)
import Label (labelW, mainLabel)
import Utils (fg, readInt, readProc)
import Volume (isMuted)

import Control.Monad (when)
import System.Environment (getEnv)
import System.Process (system)

main = mainLabel micReader
micW = clickableLeft clickCmd =<< labelW micReader

micReader = do
  muted <- isMuted "microphone"
  isLed <- isLed "micmute"
  let ledIsOn = isTrue isLed
  let ledIsOff = isFalse isLed
  when (not muted && ledIsOff) (setMute True)
  return $ fg (if muted then "black" else "red") "M"

setMute :: Bool -> IO ()
setMute val = do
  system $  "pulse-vol mic " ++ (if val then "mute" else "unmute")
  return ()

setLed :: String -> Bool -> IO ()
setLed ledName val = do
  system $  "led --set " ++ ledName ++ " " ++ (show $ fromEnum val)
  return ()

isLed :: String -> IO (Maybe Bool)
isLed ledName = do
  status <- fmap readInt $ readProc ["led", "--read", ledName]
  return $ fmap toBool status

isTrue :: Maybe Bool -> Bool
isTrue (Just True) = True
isTrue _ = False

isFalse :: Maybe Bool -> Bool
isFalse (Just False) = True
isFalse _ = False

toBool :: Integer -> Bool
toBool = toEnum . fromIntegral

clickCmd = "pulse-vol microphone toggle"
