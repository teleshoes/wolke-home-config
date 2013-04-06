module Widgets(label, clickable, clickableLeft, image, pollingImageNew) where
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk hiding (eventButton) --(toWidget, widgetShowAll, buttonNew, buttonSetImage, imageNewFromFile)
import Graphics.UI.Gtk.Gdk.Events (
  eventButton, MouseButton(LeftButton, MiddleButton, RightButton))
import System.Process (system)
import Control.Monad.Trans (liftIO)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception as E (catch, IOException)

maybeRun Nothing = return ()
maybeRun (Just cmd) = void $ forkIO $ void $ system cmd

clickCommand lCmd mCmd rCmd evt = do
  case (eventButton evt) of
    LeftButton -> maybeRun lCmd
    MiddleButton -> maybeRun mCmd
    RightButton -> maybeRun rCmd
  return False

clickableLeft w cmd = clickable w (Just cmd) Nothing Nothing

clickable w lCmd mCmd rCmd = do
  ebox <- eventBoxNew
  onButtonPress ebox $ clickCommand lCmd mCmd rCmd
  eventBoxSetVisibleWindow ebox False
  containerAdd ebox w
  widgetShowAll ebox
  return $ toWidget ebox

image file = do
  img <- imageNewFromFile file
  return $ toWidget img


pollingImageNew interval cmd = do
  img <- imageNew
  on img realize $ do
    forkIO $ forever $ do
      let tryUpdate = do
            file <- cmd
            postGUIAsync $ imageSetFromFile img file
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()
  return img

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

label interval printer = do
  w <- pollingLabelNew "---" interval printer
  widgetShowAll w
  return w
