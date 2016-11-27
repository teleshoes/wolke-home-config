module PercentBarWidget (
  percentBarWidgetW, percentBarConfig, mainPercentBarWidget
) where
import Graphics.UI.Gtk (realize, on)
import System.Taffybar.Widgets.VerticalBar (
  VerticalBarHandle,
  verticalBarNew, verticalBarSetPercent,
  BarConfig(..), defaultBarConfigIO)
import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar, threadDelay, forkIO)
import Control.Monad (forever, when, void)
import Data.Maybe (fromMaybe)

type VBColor = (Double, Double, Double)
type VBColorReader = Double -> IO VBColor

mainPercentBarWidget interval barReader = forever $ do
  (p, colors) <- barReader
  checkLen "too many colors" colors 100
  let (bg, fg) = selectColors colors p
  print (p, bg, fg)
  threadDelay $ floor $ interval * 10^6

percentBarConfig :: VBColorReader -> VBColorReader -> BarConfig
percentBarConfig bgReader fgReader = cfg { barWidth = 10
                                         , barColorIO = fgReader
                                         , barBackgroundColorIO = bgReader
                                         }
  where cfg = defaultBarConfigIO (const $ return (0, 0, 0))

percentBarWidgetW interval barReader = do
  bgColorMVar <- newEmptyMVar
  fgColorMVar <- newEmptyMVar
  let bgColorReader = colorReader bgColorMVar (0, 0, 0)
  let fgColorReader = colorReader fgColorMVar (0, 0, 0)
  (w, vbh) <- verticalBarNew $ percentBarConfig bgColorReader fgColorReader

  on w realize $ do
    void $ forkIO $ forever $ do
      (p, colors) <- barReader
      checkLen "too many colors" colors 100
      let (bg, fg) = selectColors colors p
      tryPutMVar bgColorMVar bg
      tryPutMVar fgColorMVar fg
      verticalBarSetPercent vbh (zeroToOne p)
      threadDelay $ floor $ interval * 10^6
  return w

colorReader :: MVar VBColor -> VBColor -> VBColorReader
colorReader mvar defaultColor = const $ do
  maybeColor <- tryTakeMVar mvar
  let color = fromMaybe defaultColor maybeColor
  return color

zeroToOne p | f == 0 = if n > 0 then 1.0 else 0.0
            | otherwise = f
  where (n, f) = properFraction p

checkLen msg xs len = when (length (take (len+1) xs) > len) (error msg)

selectColors :: [VBColor] -> Double -> (VBColor, VBColor)
selectColors colors p = (cs !! i, cs !! (i+1))
  where cs = cycle colors
        i = (max 0 ((ceiling p) - 1)) `mod` (length colors)
