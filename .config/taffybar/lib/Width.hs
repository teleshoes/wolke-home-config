module Width(
  widthBox, widthWrap, widthScreenWrapW, getScreenDPI) where

import Control.Monad (when)
import Graphics.X11.Xlib.Display (
  openDisplay, defaultScreen,
  displayHeight, displayWidth, displayHeightMM, displayWidthMM)
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

getScreenWidth :: IO Int
getScreenWidth = do
  d <- openDisplay ""
  return $ fromIntegral $ displayWidth d $ defaultScreen d

getScreenDPI :: IO Int
getScreenDPI = do
  d <- openDisplay ""
  let s = defaultScreen d
  let hPx = fromIntegral $ displayHeight d s
  let wPx = fromIntegral $ displayWidth d s
  let hMM = fromIntegral $ displayHeightMM d s
  let wMM = fromIntegral $ displayWidthMM d s

  let hDPI = round $ hPx / (hMM/25.4)
  let wDPI = round $ wPx / (wMM/25.4)
  when (hDPI /= wDPI) (error "horizontal/vertical DPI mismatch")

  return hDPI
