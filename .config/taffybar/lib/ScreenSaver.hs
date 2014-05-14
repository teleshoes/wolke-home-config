module ScreenSaver(screenSaverW) where
import Label (labelW)
import Utils(readProc, padL)

import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import Control.Monad (void)
import System.Process (system)

checkDelayMillis = 1 * 1000
idleTimeoutMillis = 10 * 60 * 1000

screenSaverW = do
  chan <- newChan
  writeChan chan $ "????"
  forkIO $ checkScreenSaver chan False
  labelW $ readChan chan

checkScreenSaver chan prevState = do
  xidle <- getXidle
  let state = xidle > idleTimeoutMillis
  let timeoutS = (idleTimeoutMillis - xidle) `div` 1000
  let msg = padL ' ' 4 $ if state then "SCRN" else show timeoutS
  writeChan chan msg
  if state && not prevState then screenSaverOn else return ()
  if not state && prevState then screenSaverOff else return ()

  threadDelay $ checkDelayMillis * 10^3
  checkScreenSaver chan $ state

screenSaverOn = void $ system "brightness 50"
screenSaverOff = void $ system "brightness 100"

getXidle :: IO Integer
getXidle = fmap read $ readProc ["xprintidle"]
