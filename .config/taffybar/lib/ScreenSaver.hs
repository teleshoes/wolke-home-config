module ScreenSaver(screenSaverW) where
import Label (labelW)
import Utils(chompFile, padL, readInt, readProc)

import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import System.Process (system)

overrideFile = "/tmp/screen-saver-override"

checkDelayMillis = 1 * 1000
idleTimeoutMillis = 10 * 60 * 1000

screenSaverW = do
  chan <- newChan
  writeChan chan $ "????"
  forkIO $ checkScreenSaver chan False
  labelW $ readChan chan

checkScreenSaver chan prevState = do
  xidle <- getXidle
  override <- getOverride
  let state = case override of
                "off" -> False
                _     -> xidle > idleTimeoutMillis
  let timeoutS = (idleTimeoutMillis - xidle) `div` 1000
  let msg = padL ' ' 4 $ case override of
                            "off" -> "off"
                            _     -> if state then "SCRN" else show timeoutS
  writeChan chan $ msg ++ "\nidle"
  if state && not prevState then screenSaverOn else return ()
  if not state && prevState then screenSaverOff else return ()

  threadDelay $ checkDelayMillis * 10^3
  checkScreenSaver chan $ state

getOverride = chompFile overrideFile

screenSaverOn = void $ system "brightness 50"
screenSaverOff = void $ system "brightness 100"

getXidle :: IO Integer
getXidle = fmap (fromMaybe 0 . readInt) $ readProc ["xprintidle"]
