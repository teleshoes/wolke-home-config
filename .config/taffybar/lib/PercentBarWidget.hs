module PercentBarWidget (
  percentBarWidgetW, percentBarConfig, colorMap, cycleColors
) where
import Graphics.UI.Gtk (realize, on)
import VerticalBar (
  VerticalBarHandle,
  verticalBarNew, verticalBarSetColors, verticalBarSetPercent,
  BarConfig(..), defaultBarConfig)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)

colorMap s = case s of
  "black"  -> (0.0, 0.0, 0.0)
  "gray"   -> (0.5, 0.5, 0.5)
  "red"    -> (1.0, 0.0, 0.0)
  "green"  -> (0.0, 1.0, 0.0)
  "blue"   -> (0.0, 0.0, 1.0)
  "orange" -> (1.0, 0.5, 0.0)
  "yellow" -> (1.0, 1.0, 0.0)

constColor c _ = c

cycleColors (bg:fg:[]) _ = (bg, fg)
cycleColors (bg:fg:cs) p | p > 1 = cycleColors (fg:cs) (p-1)
                         | otherwise = (bg, fg)

percent p = if p > 1 then percent (p-1) else p

percentBarConfig = cfg { barWidth = 10 }
  where cfg = defaultBarConfig (constColor (0,0,0))

percentBarWidgetW cfg interval barReader = do
  (w, vbh) <- verticalBarNew cfg
  on w realize $ do
    void $ forkIO $ forever $ do
      (fg, bg, p) <- barReader
      verticalBarSetColors vbh (constColor fg) bg
      verticalBarSetPercent vbh (percent p)
      threadDelay $ floor $ interval * 10^6
  return w
