module Clock(clockW) where
import System.Taffybar.SimpleClock (textClockNew)
import Utils (defaultDelay)

clockW = textClockNew Nothing "%a %b %d\n%H:%M:%S" defaultDelay
