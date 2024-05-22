module PingMonitor (pingMonitorW) where
import Label (labelW, mainLabel)
import Utils (defaultDelay, fg, bg, procSuccess)

import Data.Text (pack)
import System.Taffybar.Widget.Util (widgetSetClassGI)
import System.Environment (getArgs, getEnv)
import Control.Concurrent (forkIO, threadDelay,
  MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when)
import GI.Gtk.Objects.Widget (Widget)

main = do
  args <- getArgs
  when (length args /= 2) (error "Usage: PingMonitor DISPLAY URL")
  let (display, url) = (args !! 0, args !! 1)
  mainLabel =<< pingMonitorReader display url

pingMonitorW display url = do
  reader <- pingMonitorReader display url
  label <- labelW reader
  widgetSetClassGI label $ pack "PingMonitor"
  return label

charU2B24 = "⬤"
charU25CF = "●"

char = charU2B24
upColor = "green"
downColor = "red"
toggleColorTrue = "white"
toggleColorFalse = "gray"

isPingable :: String -> Int -> IO Bool
isPingable url timeout = procSuccess ["ping", url, "-c", "1", "-w", show timeout]

pingMonitorReader :: String -> String -> IO (IO String)
pingMonitorReader display url = do
  toggleMVar <- newMVar False
  let timeout = round defaultDelay
  return $ ping display url timeout toggleMVar

ping :: String -> String -> Int -> MVar Bool -> IO String
ping display url timeout toggleMVar = do
  isToggle <- takeMVar toggleMVar
  putMVar toggleMVar $ not isToggle

  isUp <- isPingable url timeout

  let color = if isUp then "green" else "red"
  let toggleColor = if isToggle then toggleColorTrue else toggleColorFalse

  return $ bg "black" $ fg color char ++ fg toggleColor display
