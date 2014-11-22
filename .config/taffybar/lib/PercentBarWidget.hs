module PercentBarWidget (
  percentBarWidgetW, percentBarConfig, mainPercentBarWidget
) where
import Graphics.UI.Gtk (realize, on)
import System.Taffybar.Widgets.VerticalBar (
  VerticalBarHandle,
  verticalBarNew, verticalBarSetColors, verticalBarSetPercent,
  BarConfig(..), defaultBarConfig)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, when, void)

mainPercentBarWidget interval barReader = forever $ do
  (p, colors) <- barReader
  checkLen "too many colors" colors 100
  let (bg, fg) = selectColors colors p
  print (p, bg, fg)
  threadDelay $ floor $ interval * 10^6

percentBarConfig = cfg { barWidth = 10 }
  where cfg = defaultBarConfig (constColor (0,0,0))

percentBarWidgetW cfg interval barReader = do
  (w, vbh) <- verticalBarNew cfg
  on w realize $ do
    void $ forkIO $ forever $ do
      (p, colors) <- barReader
      checkLen "too many colors" colors 100
      let (bg, fg) = selectColors colors p
      verticalBarSetColors vbh (constColor fg) (constColor bg)
      verticalBarSetPercent vbh (zeroToOne p)
      threadDelay $ floor $ interval * 10^6
  return w

zeroToOne p | f == 0 = if n > 0 then 1.0 else 0.0
            | otherwise = f
  where (n, f) = properFraction p

checkLen msg xs len = when (length (take (len+1) xs) > len) (error msg)

constColor c _ = c

selectColors colors p = (cs !! i, cs !! (i+1))
  where cs = cycle colors
        i = (max 0 ((ceiling p) - 1)) `mod` (length colors)
