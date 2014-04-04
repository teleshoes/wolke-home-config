module PingMonitor (pingMonitorW, pingLabelW) where
import Label (labelW)
import Utils (defaultDelay, fg, rowW, colW, procSuccess)

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

pingMonitorW :: [[(String, String)]] -> IO Widget
pingMonitorW urlDisplayColumns = do
  columns <- mapM pingMonitorColW urlDisplayColumns
  rowW columns

pingMonitorColW :: [(String, String)] -> IO Widget
pingMonitorColW urlDisplays = do
  monitors <- mapM (\(url, display) -> pingLabelW (url, display)) urlDisplays
  colW monitors

pingLabelW :: (String, String) -> IO Widget
pingLabelW (url, display) = do
  chan <- newChan
  writeChan chan $ "?" ++ display
  let toggle = cycle [True, False]
  forkIO $ mapM_ (ping chan url display defaultDelay) toggle
  labelW $ readChan chan

ping chan url display timeout toggle = do
  isUp <- isPingable url timeout
  let wait = if isUp then 3 else 1

  let color = if isUp then "green" else "red"
  let toggleColor = if toggle then toggleColorTrue else toggleColorFalse

  writeChan chan $ fg color char ++ fg toggleColor display

  threadDelay $ wait * 10^6
