module Width(widthBox, widthWrap, widthScreenWrapW) where

import Graphics.X11.Xlib.Display (openDisplay, defaultScreen, displayWidth)
import Graphics.UI.Gtk (
  Container, Widget, hBoxNew, widgetSetSizeRequest, containerAdd, toWidget, toContainer, widgetShowAll)

widthBox :: Int -> IO Container
widthBox widthPx = do
  wbox <- hBoxNew False 0
  widgetSetSizeRequest wbox widthPx (-1)
  widgetShowAll wbox
  return $ toContainer wbox

widthWrap :: Int -> Widget -> IO Container
widthWrap widthPx w = do
  wbox <- widthBox widthPx
  containerAdd wbox w
  widgetSetSizeRequest w widthPx (-1)
  widgetShowAll wbox
  return wbox

widthScreenWrapW :: Double -> Widget -> IO Widget
widthScreenWrapW screenRatio w = do
  screenWidthPx <- getScreenWidth
  let widthPx = round $ screenRatio * fromIntegral screenWidthPx
  fmap toWidget $ widthWrap widthPx w

getScreenWidth :: IO Integer
getScreenWidth = do
  d <- openDisplay ""
  return $ fromIntegral $ displayWidth d $ defaultScreen d
