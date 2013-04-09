module Widgets(
  clickableActions, clickableAsync, clickableLeftAsync,
  clickable, clickableLeft,
  label, image, pollingImageNew
) where
import Utils (defaultDelay)

import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk (
  Widget, WidgetClass,
  imageNew, imageNewFromFile, imageSetFromFile,
  eventBoxNew, eventBoxSetVisibleWindow, onButtonPress,
  containerAdd, toWidget, on, realize, widgetShowAll, postGUIAsync)
import Graphics.UI.Gtk.Gdk.Events (
  eventButton, Event, MouseButton(LeftButton, MiddleButton, RightButton))
import System.Process (system)
import Control.Monad.Trans (liftIO)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception as E (catch, IOException)

type Act = IO ()
type CmdA = IO (Maybe String)
type Cmd = Maybe String

maybeRun :: Cmd -> IO ()
maybeRun Nothing = return ()
maybeRun (Just cmd) = void $ forkIO $ void $ system cmd

handleClickAction :: Act -> Act -> Act -> Event -> IO Bool
handleClickAction lAct mAct rAct evt = do
  case (eventButton evt) of
    LeftButton -> lAct
    MiddleButton -> mAct
    RightButton -> rAct
  return False

clickableActions :: (WidgetClass w) => Act -> Act -> Act -> w -> IO Widget
clickableActions lAct mAct rAct w = do
  ebox <- eventBoxNew
  onButtonPress ebox $ handleClickAction lAct mAct rAct
  eventBoxSetVisibleWindow ebox False
  containerAdd ebox w
  widgetShowAll ebox
  return $ toWidget ebox

clickableAsync :: (WidgetClass w) => CmdA -> CmdA -> CmdA -> w -> IO Widget
clickableAsync lCmdA mCmdA rCmdA w = clickableActions l m r w
  where (l,m,r) = (maybeRun =<< lCmdA, maybeRun =<< mCmdA, maybeRun =<< rCmdA)

clickableLeftAsync :: (WidgetClass w) => CmdA -> w -> IO Widget
clickableLeftAsync cmdA w = clickableAsync l m r w
  where (l,m,r) = (cmdA, return Nothing, return Nothing)

clickable :: (WidgetClass w) => Cmd -> Cmd -> Cmd -> w -> IO Widget
clickable lCmd mCmd rCmd w = clickableAsync l m r w
  where (l,m,r) = (return lCmd, return mCmd, return rCmd)

clickableLeft :: (WidgetClass w) => String -> w -> IO Widget
clickableLeft cmd w = clickableAsync l m r w
  where (l,m,r) = (return $ Just cmd, return Nothing, return Nothing)




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
