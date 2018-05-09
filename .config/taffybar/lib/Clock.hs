module Clock(clockW) where
import Graphics.UI.Gtk (Widget)
import System.Taffybar.Widget.SimpleClock (textClockNew)
import Utils (defaultDelay)

clockW :: IO Widget
clockW = textClockNew Nothing "%a %b %d\n%H:%M:%S" defaultDelay
