module PercentBarWidget (
  percentBarWidgetW, percentBarConfig, cycleColors
) where
import Graphics.UI.Gtk (realize, on)
import System.Taffybar.Widgets.VerticalBar (
  VerticalBarHandle,
  verticalBarNew, verticalBarSetColors, verticalBarSetPercent,
  BarConfig(..), defaultBarConfig)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)

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
