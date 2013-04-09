module Widgets(
  clickableAsync, clickableLeftAsync, clickable, clickableLeft,
  label, image, pollingImageNew
) where
import Utils (defaultDelay)

import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk (
  imageNew, imageNewFromFile, imageSetFromFile,
  eventBoxNew, eventBoxSetVisibleWindow, onButtonPress,
  containerAdd, toWidget, on, realize, widgetShowAll, postGUIAsync)
import Graphics.UI.Gtk.Gdk.Events (
  eventButton, MouseButton(LeftButton, MiddleButton, RightButton))
import System.Process (system)
import Control.Monad.Trans (liftIO)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception as E (catch, IOException)

maybeRun Nothing = return ()
maybeRun (Just cmd) = void $ forkIO $ void $ system cmd

clickCommandAsync lCmdAsync mCmdAsync rCmdAsync evt = do
  lCmd <- lCmdAsync
  mCmd <- mCmdAsync
  rCmd <- rCmdAsync
  case (eventButton evt) of
    LeftButton -> maybeRun lCmd
    MiddleButton -> maybeRun mCmd
    RightButton -> maybeRun rCmd
  return False

clickableAsync lCmdAsync mCmdAsync rCmdAsync w = do
  ebox <- eventBoxNew
  onButtonPress ebox $ clickCommandAsync lCmdAsync mCmdAsync rCmdAsync
  eventBoxSetVisibleWindow ebox False
  containerAdd ebox w
  widgetShowAll ebox
  return $ toWidget ebox

clickableLeftAsync cmdAsync w = clickableAsync l c r w
  where (l,c,r) = (cmdAsync, return Nothing, return Nothing)
clickable lCmd mCmd rCmd w = clickableAsync l c r w
  where (l,c,r) = (return lCmd, return mCmd, return rCmd)
clickableLeft cmd w = clickableAsync l c r w
  where (l,c,r) = (return $ Just cmd, return Nothing, return Nothing)

image file = do
  img <- imageNewFromFile file
  return $ toWidget img


pollingImageNew cmd = do
  img <- imageNew
  on img realize $ do
    forkIO $ forever $ do
      let tryUpdate = do
            file <- cmd
            postGUIAsync $ imageSetFromFile img file
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (defaultDelay * 1000000)
    return ()
  return img

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

label printer = do
  w <- pollingLabelNew "---" defaultDelay printer
  widgetShowAll w
  return w
