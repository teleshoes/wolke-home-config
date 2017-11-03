module Clickable(
  clickableActions,
  clickableAsync, clickableLeftAsync, clickableMiddleAsync, clickableRightAsync,
  clickable, clickableLeft, clickableMiddle, clickableRight
) where

import Graphics.UI.Gtk (
  Widget, WidgetClass,
  eventBoxNew, eventBoxSetVisibleWindow,
  containerAdd, toWidget, widgetShowAll,
  on, buttonPressEvent)
import Graphics.UI.Gtk.Gdk.EventM (
  eventButton, EButton, EventM, MouseButton(LeftButton, MiddleButton, RightButton))
import System.Process (system)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception as E (catch, IOException)

type Act = IO ()
type CmdA = IO (Maybe String)
type Cmd = Maybe String

maybeRun :: Cmd -> IO ()
maybeRun Nothing = return ()
maybeRun (Just cmd) = void $ forkIO $ void $ system cmd

handleClickAction :: Act -> Act -> Act -> EventM EButton Bool
handleClickAction lAct mAct rAct = do
  btn <- eventButton
  liftIO $ case btn of
    LeftButton -> lAct
    MiddleButton -> mAct
    RightButton -> rAct
  return False

clickableActions :: (WidgetClass w) => Act -> Act -> Act -> w -> IO Widget
clickableActions lAct mAct rAct w = do
  ebox <- eventBoxNew
  on ebox buttonPressEvent $ handleClickAction lAct mAct rAct
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

clickableMiddleAsync :: (WidgetClass w) => CmdA -> w -> IO Widget
clickableMiddleAsync cmdA w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, cmdA, return Nothing)

clickableRightAsync :: (WidgetClass w) => CmdA -> w -> IO Widget
clickableRightAsync cmdA w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, return Nothing, cmdA)

clickable :: (WidgetClass w) => Cmd -> Cmd -> Cmd -> w -> IO Widget
clickable lCmd mCmd rCmd w = clickableAsync l m r w
  where (l,m,r) = (return lCmd, return mCmd, return rCmd)

clickableLeft :: (WidgetClass w) => String -> w -> IO Widget
clickableLeft cmd w = clickableAsync l m r w
  where (l,m,r) = (return $ Just cmd, return Nothing, return Nothing)

clickableMiddle :: (WidgetClass w) => String -> w -> IO Widget
clickableMiddle cmd w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, return $ Just cmd, return Nothing)

clickableRight :: (WidgetClass w) => String -> w -> IO Widget
clickableRight cmd w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, return Nothing, return $ Just cmd)
