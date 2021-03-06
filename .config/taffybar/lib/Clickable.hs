module Clickable(
  clickableActions,
  clickableAsync, clickableLeftAsync, clickableMiddleAsync, clickableRightAsync,
  clickable, clickableLeft, clickableMiddle, clickableRight
) where

import GI.Gdk.Structs.EventButton (
  EventButton,
  getEventButtonButton)
import Data.GI.Base.BasicTypes (GObject)
import Data.GI.Base.Overloading (IsDescendantOf)
import GI.Gtk.Objects.Widget (
  Widget,
  onWidgetButtonPressEvent, toWidget, widgetShowAll)
import GI.Gtk.Objects.Container (
  containerAdd)
import GI.Gtk.Objects.EventBox (
  eventBoxNew, eventBoxSetVisibleWindow)
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

handleClickAction :: Act -> Act -> Act -> EventButton -> IO Bool
handleClickAction lAct mAct rAct btn = do
  mouseBtn <- getEventButtonButton btn
  case mouseBtn of
    1 -> lAct
    2 -> mAct
    3 -> rAct
  return False

clickableActions :: (GObject w, IsDescendantOf Widget w) => Act -> Act -> Act -> w -> IO Widget
clickableActions lAct mAct rAct w = do
  ebox <- eventBoxNew
  onWidgetButtonPressEvent ebox $ handleClickAction lAct mAct rAct
  eventBoxSetVisibleWindow ebox False
  containerAdd ebox w
  widgetShowAll ebox
  w <- toWidget ebox
  return w

clickableAsync :: (GObject w, IsDescendantOf Widget w) => CmdA -> CmdA -> CmdA -> w -> IO Widget
clickableAsync lCmdA mCmdA rCmdA w = clickableActions l m r w
  where (l,m,r) = (maybeRun =<< lCmdA, maybeRun =<< mCmdA, maybeRun =<< rCmdA)

clickableLeftAsync :: (GObject w, IsDescendantOf Widget w) => CmdA -> w -> IO Widget
clickableLeftAsync cmdA w = clickableAsync l m r w
  where (l,m,r) = (cmdA, return Nothing, return Nothing)

clickableMiddleAsync :: (GObject w, IsDescendantOf Widget w) => CmdA -> w -> IO Widget
clickableMiddleAsync cmdA w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, cmdA, return Nothing)

clickableRightAsync :: (GObject w, IsDescendantOf Widget w) => CmdA -> w -> IO Widget
clickableRightAsync cmdA w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, return Nothing, cmdA)

clickable :: (GObject w, IsDescendantOf Widget w) => Cmd -> Cmd -> Cmd -> w -> IO Widget
clickable lCmd mCmd rCmd w = clickableAsync l m r w
  where (l,m,r) = (return lCmd, return mCmd, return rCmd)

clickableLeft :: (GObject w, IsDescendantOf Widget w) => String -> w -> IO Widget
clickableLeft cmd w = clickableAsync l m r w
  where (l,m,r) = (return $ Just cmd, return Nothing, return Nothing)

clickableMiddle :: (GObject w, IsDescendantOf Widget w) => String -> w -> IO Widget
clickableMiddle cmd w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, return $ Just cmd, return Nothing)

clickableRight :: (GObject w, IsDescendantOf Widget w) => String -> w -> IO Widget
clickableRight cmd w = clickableAsync l m r w
  where (l,m,r) = (return Nothing, return Nothing, return $ Just cmd)
