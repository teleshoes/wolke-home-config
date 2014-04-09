module PingMonitor (pingMonitorW) where
import Label (labelW)
import Utils (defaultDelay, fg, bg, procSuccess)

import System.Environment (getEnv)
import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import System.Environment.UTF8 (getArgs)
import Graphics.UI.Gtk (Widget)

char = "●"
upColor = "green"
downColor = "red"
toggleColorTrue = "white"
toggleColorFalse = "gray"

isPingable url timeout = procSuccess ["ping", url, "-c", "1", "-w", show timeout]

pingMonitorW :: String -> String -> IO Widget
pingMonitorW display url = do
  chan <- newChan
  writeChan chan $ "?" ++ display
  let toggle = cycle [True, False]
  forkIO $ mapM_ (ping chan display url defaultDelay) toggle
  labelW $ readChan chan

ping chan display url timeout toggle = do
  isUp <- isPingable url timeout
  let wait = if isUp then 3 else 1

  let color = if isUp then "green" else "red"
  let toggleColor = if toggle then toggleColorTrue else toggleColorFalse

  writeChan chan $ bg "black" $ fg color char ++ fg toggleColor display

  threadDelay $ wait * 10^6
