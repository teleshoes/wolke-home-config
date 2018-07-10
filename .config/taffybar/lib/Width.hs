module Width(
  widthBox, widthWrap, widthScreenWrapW, widthCharWrapW,
  screenPctToPx, charsFitInPx, getScreenDPI) where

import Control.Monad (when)
import Graphics.X11.Xlib.Display (
  openDisplay, defaultScreen,
  displayHeight, displayWidth, displayHeightMM, displayWidthMM)

import GI.Gtk.Enums (
  Orientation(OrientationHorizontal))
import GI.Gtk.Objects.Box (boxNew, boxSetHomogeneous)
import GI.Gtk.Objects.Container (
  Container, containerAdd, toContainer)
import GI.Gtk.Objects.Widget (
  Widget, toWidget, widgetSetSizeRequest, widgetShowAll)

-- ratio of font size to width
fontSizeToWidthRatio = 2.0 -- Inconsolata medium

-- font size in points to font size in pixels
fontSizePtToPx :: Int -> Double -> Double
fontSizePtToPx dpi fontSizePt = fontSizeIn * fromIntegral dpi
  where fontSizeIn = fontSizePt / 72.0

-- font size in points to character width in pixels
charWidth :: Int -> Double -> Int
charWidth dpi fontSizePt = ceiling $ fontSizePx / fontSizeToWidthRatio
  where fontSizePx = fontSizePtToPx dpi fontSizePt

widthBox :: Int -> IO Container
widthBox widthPx = do
  wbox <- boxNew OrientationHorizontal 0
  boxSetHomogeneous wbox False

  widgetSetSizeRequest wbox (fromIntegral widthPx) (-1)
  widgetShowAll wbox
  toContainer wbox

widthWrap :: Int -> Widget -> IO Container
widthWrap widthPx w = do
  wbox <- widthBox widthPx
  containerAdd wbox w
  widgetSetSizeRequest w (fromIntegral widthPx) (-1)
  widgetShowAll wbox
  return wbox

widthScreenWrapW :: Double -> Widget -> IO Widget
widthScreenWrapW screenRatio w = do
  screenWidthPx <- getScreenWidth
  let widthPx = round $ screenRatio * fromIntegral screenWidthPx
  box <- widthWrap widthPx w
  toWidget box

widthCharWrapW :: Int -> Double -> Int -> Widget -> IO Widget
widthCharWrapW dpi fontSize charCount w = do
  let widthPx = charWidth dpi fontSize * charCount
  box <- widthWrap widthPx w
  toWidget box

screenPctToPx :: Double -> IO Int
screenPctToPx pct = do
  w <- getScreenWidth
  return $ round $ (fromIntegral w) * pct/100.0

charsFitInPx :: Int -> Double -> Int -> Int
charsFitInPx dpi fontSizePt px = floor $ (fromIntegral px)/(fromIntegral charW)
  where charW = charWidth dpi fontSizePt

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

  let hDPI = hPx / (hMM/25.4)
  let wDPI = wPx / (wMM/25.4)
  let avgDPI = (hDPI + wDPI) / 2

  let diff = hDPI - wDPI
  when (abs diff > 1) $ error $ ""
                                ++ "horizontal/vertical DPI mismatch:\n"
                                ++ "  hor=" ++ show hDPI ++ "\n"
                                ++ "  ver=" ++ show wDPI ++ "\n"

  return $ round avgDPI
