module ScreenSaver(screenSaverW) where
import Clickable (clickableActions)
import Label (labelW, mainLabel)
import Utils(chompFile, padL, readInt, readProc, millisTime)

import Control.Concurrent (forkIO, threadDelay,
  MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when, void)
import Data.Maybe (fromMaybe)
import System.Process (system)

main = mainLabel =<< (screenSaverReader False)
screenSaverW isBot = clickableActions clickL clickM clickR =<< labelW =<< (screenSaverReader isBot)

clickL = writeOverride "on"
clickM = return ()
clickR = writeOverride "off"

screenSaverBrightness = 25

overrideFile = "/tmp/screen-saver-override"

-- screensaver timeout
idleTimeoutMillis = 10 * 60 * 1000
-- minimum amount of time to run screensaver when forcibly turning it on
minRunningMillis = 2 * 1000
-- time before swapping taffybar position --top/--bottom
taffybarSwapDelayMillis = 60 * 60 * 1000 --1h

screenSaverReader :: Bool -> IO (IO String)
screenSaverReader isBottom = do
  stateMVar <- newMVar (isBottom, False, 0, Nothing)
  return $ checkScreenSaver stateMVar

checkScreenSaver :: MVar (Bool, Bool, Integer, Maybe Integer) -> IO String
checkScreenSaver stateMVar = do
  (isBottom, prevState, prevXidle, prevStartTimeMillis) <- takeMVar stateMVar
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

  if state && not prevState then screenSaverOn else return ()
  if not state && prevState then screenSaverOff else return ()

  if override == "on" && not state then writeOverride "" else return ()

  when (state) $ maybeTaffybarSwap isBottom $ runningMillis
  when (not state && isBottom) $ taffybarSwap False

  putMVar stateMVar (isBottom, state, xidle, startTime)
  return $ msg ++ "\nidle"


getOverride = chompFile overrideFile

writeOverride state = writeFile overrideFile $ state ++ "\n"

screenSaverOn = do
  hhpc True
  void $ system $ "brightness " ++ show screenSaverBrightness
screenSaverOff = do
  hhpc False
  void $ system "brightness 100"

hhpc on = do
  void $ system "pkill hhpc"
  when on $ void $ system "hhpc &"

getXidle :: IO Integer
getXidle = fmap (fromMaybe 0 . readInt) $ readProc ["xprintidle"]

maybeTaffybarSwap :: Bool -> Integer -> IO ()
maybeTaffybarSwap currentIsBottom elapsedActiveMillis = do
  when (elapsedActiveMillis > taffybarSwapDelayMillis) $ taffybarSwap $ not currentIsBottom

taffybarSwap :: Bool -> IO ()
taffybarSwap targetIsBottom = void $ system $ "taffybar-swap " ++ topBotArg
  where topBotArg = if targetIsBottom then "--bottom" else "--top"
