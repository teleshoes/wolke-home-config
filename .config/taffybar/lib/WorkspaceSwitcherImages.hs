--WorkspaceSwitcherImages, Copyright 2013 by Elliot Wolk
--Based on WorkspaceSwitcher, Copyright 2012 by José A. Romero L.

module WorkspaceSwitcherImages (
  -- * Usage
  -- $usage
  workspaceSwitcherImagesNew
) where

import WorkspaceImages (loadImages, selectImage)

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Graphics.UI.Gtk
import Graphics.X11.Xlib.Extras

import System.Environment (getEnv)

import System.Taffybar.Pager
import System.Taffybar.WindowSwitcher (windowSwitcherNew)
import System.Information.EWMHDesktopInfo
import System.Information.X11DesktopInfo

type Desktop = ([Workspace], [(String, Maybe Pixbuf)])
type Workspace = (Label, Image, String)

wsBorderColor = Color 65535 0 0

-- $usage
-- Display clickable workspace labels and images based on window title/class.
--
-- This widget requires that the EwmhDesktops hook from the XMonadContrib
-- project be installed in your @xmonad.hs@ file:
--
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- > main = do
-- >   xmonad $ ewmh $ defaultConfig
-- > ...
--
-- Urgency hooks are not required for the urgency hints displaying to work
-- (since it is also based on desktop events), but if you use @focusUrgent@
-- you may want to keep the \"@withUrgencyHook NoUrgencyHook@\" anyway.
--
-- Unfortunately, in multiple monitor installations EWMH does not provide a
-- way to determine what desktops are shown in secondary displays. Thus, if
-- you have more than one monitor you may want to additionally install the
-- "System.Taffybar.Hooks.PagerHints" hook in your @xmonad.hs@:
--
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.WorkspaceSwitcher
-- > main = do
-- >   pager <- pagerNew defaultPagerConfig
-- >   let wss = workspaceSwitcherImagesNew pager
--
-- now you can use it as any other Taffybar widget.

getWorkspaces :: Desktop -> [Workspace]
getWorkspaces = fst

getPixbufs :: Desktop -> [(String, Maybe Pixbuf)]
getPixbufs = snd

getWs :: Desktop -> Int -> Workspace
getWs (wss, _) i = wss !! i

lbl :: Workspace -> Label
lbl (x,_,_) = x
img :: Workspace -> Image
img (_,x,_) = x
name :: Workspace -> String
name (_,_,x) = x

-- | Create a new WorkspaceSwitcher widget that will use the given Pager as
-- its source of events.
workspaceSwitcherImagesNew :: Pager -> IO Widget
workspaceSwitcherImagesNew pager = do
  desktop <- getDesktop
  widget  <- assembleWidget desktop
  idxRef  <- newIORef []
  let cfg = config pager
      activecb = activeCallback cfg desktop idxRef
      urgentcb = urgentCallback cfg desktop
  subscribe pager activecb "_NET_CURRENT_DESKTOP"
  subscribe pager urgentcb "WM_HINTS"
  return widget

-- | Return a pair containing a list of workspaces
-- and a list of pairs of imagename to pixbuf.
-- Workspaces are three-element tuples,
-- containing the Label widget used to display the name of that specific
-- workspace, an image to display for that workspace,
-- and a String with the workspace name.
getDesktop :: IO Desktop
getDesktop = do
  names  <- withDefaultCtx getWorkspaceNames
  labels <- toLabels names
  images <- toImages names
  pixbufs <- loadImages
  return $ (zip3 labels images names, pixbufs)

getProp :: X11Window -> String -> X11Property String
getProp window prop = readAsString (Just window) prop

getActiveWindow = do
  awt <- readAsListOfWindow Nothing "_NET_ACTIVE_WINDOW"
  return $ case awt of
    w:ws      -> if w > 0 then Just w else Nothing
    otherwise -> Nothing

getActiveProp :: String -> X11Property String
getActiveProp prop = do
  w <- getActiveWindow
  case w of
    Nothing -> return ""
    Just win -> getProp win prop

getActiveTitle = do
  net <- getActiveProp "_NET_WM_NAME"
  case net of
    "" -> getActiveProp "_WM_NAME"
    _  -> return net

getActiveClass = getActiveProp "WM_CLASS"


-- | Build the graphical representation of the widget.
assembleWidget :: Desktop -> IO Widget
assembleWidget desktop = do
  hbox <- hBoxNew True 3
  mapM_ (addButton hbox desktop) $ [0..(length (getWorkspaces desktop) - 1)]
  widgetShowAll hbox
  return $ toWidget hbox

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_CURRENT_DESKTOP" standard events. It will track the position of
-- the active workspace in the desktop.
activeCallback :: PagerConfig -> Desktop -> IORef [Int] -> Event -> IO ()
activeCallback cfg desktop ref _ = do
  prev <- readIORef ref
  curr <- withDefaultCtx getVisibleWorkspaces
  transition cfg desktop prev curr
  writeIORef ref curr

-- | Build a suitable callback function that can be registered as Listener
-- of "WM_HINTS" standard events. It will display in a different color any
-- workspace (other than the active one) containing one or more windows
-- with its urgency hint set.
urgentCallback :: PagerConfig -> Desktop -> Event -> IO ()
urgentCallback cfg desktop event = withDefaultCtx $ do
  let window = ev_window event
  isUrgent <- isWindowUrgent window
  when isUrgent $ do
    this <- getCurrentWorkspace
    that <- getWorkspace window
    when (this /= that) $ do
      liftIO $ mark desktop (urgentWorkspace cfg) that

-- | Convert the given list of Strings to a list of Label widgets.
toLabels :: [String] -> IO [Label]
toLabels = sequence . map (labelNew . Just)

toImages :: [String] -> IO [Image]
toImages = mapM (\_ -> imageNew)

-- | Build a new clickable event box containing the Label widget that
-- corresponds to the given index, and add it to the given container.
addButton :: HBox    -- ^ Graphical container.
          -> Desktop -- ^ List of all workspaces available.
          -> Int     -- ^ Index of the workspace to use.
          -> IO ()
addButton hbox desktop idx = do
  let ws = getWs desktop idx
  wsbox <- hBoxNew False 0
  containerAdd wsbox $ lbl ws
  containerAdd wsbox $ img ws
  frame <- frameNew
  widgetModifyBg frame StateNormal wsBorderColor
  containerAdd frame wsbox
  ebox <- eventBoxNew
  on ebox buttonPressEvent $ switch idx
  containerAdd ebox frame
  containerAdd hbox ebox

-- | Perform all changes needed whenever the active workspace changes.
transition :: PagerConfig -- ^ Configuration settings.
           -> Desktop -- ^ All available Labels with their default values.
           -> [Int] -- ^ Previously visible workspaces (first was active).
           -> [Int] -- ^ Currently visible workspaces (first is active).
           -> IO ()
transition cfg desktop prev curr = do
  markImg desktop (head curr)
  when (curr /= prev) $ do
    mapM_ (mark desktop id) prev
    mark desktop (activeWorkspace cfg) (head curr)
    mapM_ (mark desktop $ visibleWorkspace cfg) (tail curr)

markImg :: Desktop -> Int -> IO ()
markImg desktop idx = do
  let ws = getWs desktop idx
  pixbuf <- withDefaultCtx $ do
    winTitle <- getActiveTitle
    winClass <- getActiveClass
    return $ selectImage (getPixbufs desktop) winTitle winClass
  postGUIAsync $ case pixbuf of
    Just pb -> imageSetFromPixbuf (img ws) pb
    Nothing -> imageClear (img ws)

-- | Apply the given marking function to the Label of the workspace with
-- the given index.
mark :: Desktop -- ^ List of all available workspaces.
     -> (String -> Markup) -- ^ Marking function.
     -> Int -- ^ Index of the Label to modify.
     -> IO ()
mark desktop decorate idx = do
  let ws = getWs desktop idx
  postGUIAsync $ labelSetMarkup (lbl ws) $ decorate $ name ws

-- | Switch to the workspace with the given index.
switch :: (MonadIO m) => Int -> m Bool
switch idx = do
  liftIO $ withDefaultCtx (switchToWorkspace idx)
  return True
