module ScreenSaver(screenSaverW) where
import Clickable (clickableActions)
import Label (labelW, mainLabel)
import Utils(padL, readInt, readProc, regexGroups)

import Control.Monad (when, void)
import Data.Maybe (fromMaybe)
import System.Process (system)

main = mainLabel (screenSaverReader False)
screenSaverW isBot = clickableActions clickL clickM clickR =<< labelW (screenSaverReader isBot)

clickL = void $ system "screensaver --on"
clickM = return ()
clickR = void $ system "screensaver --disable"

data ScreensaverInfo = ScreensaverInfo
  { state               :: String
  , elapsedActiveMillis :: Integer
  , currentIdleMillis   :: Integer
  , idleTimeoutMillis   :: Integer
  }

-- time before swapping taffybar position --top/--bottom
taffybarSwapDelayMillis = 60 * 60 * 1000 --1h

screenSaverReader :: Bool -> IO String
screenSaverReader isBottom = do
  info <- fmap parseScreensaverInfo $ readProc ["screensaver", "--info"]
  let remainingS = (idleTimeoutMillis info - currentIdleMillis info) `div` 1000
  let msg = case state info of
              "disabled" -> "KEEP\nBRGT"
              "off"      -> (padL ' ' 4 $ show remainingS) ++ "\nidle"
              "on"       -> "scrn\ndim "
              _          -> "????\n????"

  when (state info == "on") $ maybeTaffybarSwap isBottom $ elapsedActiveMillis info
  when (state info /= "on" && isBottom) $ taffybarSwap False

  return msg

parseScreensaverInfo :: String -> ScreensaverInfo
parseScreensaverInfo info = ScreensaverInfo
                              state
                              (forceInt elapsedActiveMillis)
                              (forceInt currentIdleMillis)
                              (forceInt idleTimeoutMillis)
  where re = ""
             ++ "STATE=(\\w+)\n"
             ++ "ELAPSED_ACTIVE_MILLIS=(\\d+)\n"
             ++ "CURRENT_IDLE_MILLIS=(\\d+)\n"
             ++ "IDLE_TIMEOUT_MILLIS=(\\d+)\n"
        matchGroups = fromMaybe ["UNKNOWN", "0", "0", "0"] $ regexGroups re info
        [state, elapsedActiveMillis, currentIdleMillis, idleTimeoutMillis] = matchGroups
        forceInt = (fromMaybe 0) . readInt

maybeTaffybarSwap :: Bool -> Integer -> IO ()
maybeTaffybarSwap currentIsBottom elapsedActiveMillis = do
  let elapsedActiveSwapPeriods = (elapsedActiveMillis `div` taffybarSwapDelayMillis)
  let newIsBottom = elapsedActiveSwapPeriods`mod`2 == 1 --odd periods are bottom
  when (currentIsBottom /= newIsBottom) $ taffybarSwap newIsBottom

taffybarSwap :: Bool -> IO ()
taffybarSwap targetIsBottom = void $ system $ "taffybar-swap " ++ topBotArg
  where topBotArg = if targetIsBottom then "--bottom" else "--top"
