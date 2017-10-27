module PingMonitor (pingMonitorW) where
import Label (labelW, mainLabel)
import Utils (defaultDelay, fg, bg, procSuccess, widgetSetClass)

import System.Environment (getArgs, getEnv)
import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import Control.Monad (when)
import Graphics.UI.Gtk (Widget)

main = do
  args <- getArgs
  when (length args /= 2) (error "Usage: PingMonitor DISPLAY URL")
  let (display, url) = (args !! 0, args !! 1)
  mainLabel =<< pingMonitorReader display url

pingMonitorW display url = do
  reader <- pingMonitorReader display url
  label <- labelW reader
  widgetSetClass label "PingMonitor"
  return label

char = "●"
upColor = "green"
downColor = "red"
toggleColorTrue = "white"
toggleColorFalse = "gray"

isPingable url timeout = procSuccess ["ping", url, "-c", "1", "-w", show timeout]

pingMonitorReader :: String -> String -> IO (IO String)
pingMonitorReader display url = do
  chan <- newChan
  writeChan chan $ "?" ++ display
  let toggle = cycle [True, False]
  forkIO $ mapM_ (ping chan display url defaultDelay) toggle
  return $ readChan chan

ping chan display url timeout toggle = do
  isUp <- isPingable url timeout
  let wait = if isUp then 3 else 1

  let color = if isUp then "green" else "red"
  let toggleColor = if toggle then toggleColorTrue else toggleColorFalse

  writeChan chan $ bg "black" $ fg color char ++ fg toggleColor display

  threadDelay $ wait * 10^6
