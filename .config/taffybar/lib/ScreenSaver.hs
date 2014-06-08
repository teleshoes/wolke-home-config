module ScreenSaver(screenSaverW) where
import Clickable (clickableActions)
import Label (labelW)
import Utils(chompFile, padL, readInt, readProc, millisTime)

import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import System.Process (system)

clickL = writeOverride "on"
clickM = return ()
clickR = writeOverride "off"

screenSaverBrightness = 5

overrideFile = "/tmp/screen-saver-override"

-- screensaver timeout
idleTimeoutMillis = 10 * 60 * 1000
-- delay between runs
checkDelayMillis = 1 * 1000
-- minimum amount of time to run screensaver when forcibly turning it on
minRunningMillis = 2 * 1000

screenSaverW = do
  chan <- newChan
  writeChan chan $ "????"
  forkIO $ checkScreenSaver chan False 0 Nothing
  label <- labelW $ readChan chan
  clickableActions clickL clickM clickR label

checkScreenSaver chan prevState prevXidle prevStartTimeMillis = do
  xidle <- getXidle
  override <- getOverride
  nowMillis <- millisTime
  let runningMillis = nowMillis - fromMaybe nowMillis prevStartTimeMillis

  let state = case override of
                "off" -> False
                "on"  -> runningMillis < minRunningMillis || xidle > prevXidle
                _     -> xidle > idleTimeoutMillis
  let startTime = if state
                  then Just $ fromMaybe nowMillis prevStartTimeMillis
                  else Nothing

  let timeoutS = (idleTimeoutMillis - xidle) `div` 1000
  let msg = padL ' ' 4 $ case override of
                            "off" -> "off"
                            "on"  -> "on"
                            _     -> if state then "SCRN" else show timeoutS

  writeChan chan $ msg ++ "\nidle"
  if state && not prevState then screenSaverOn else return ()
  if not state && prevState then screenSaverOff else return ()

  if override == "on" && not state then writeOverride "" else return ()

  threadDelay $ checkDelayMillis * 10^3
  checkScreenSaver chan state xidle startTime


getOverride = chompFile overrideFile

writeOverride state = writeFile overrideFile $ state ++ "\n"

screenSaverOn = void $ system $ "brightness " ++ show screenSaverBrightness
screenSaverOff = void $ system "brightness 100"

getXidle :: IO Integer
getXidle = fmap (fromMaybe 0 . readInt) $ readProc ["xprintidle"]
