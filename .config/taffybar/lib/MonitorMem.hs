module MonitorMem(monitorMemW) where
import Color as C
import Utils (pollingGraphMain)
import System.Information.Memory (parseMeminfo, memoryUsedRatio)
import System.Taffybar.Widgets.PollingGraph (
  pollingGraphNew, defaultGraphConfig, graphWidth, graphDataColors,
  graphDirection, GraphDirection(..))

main = pollingGraphMain 1 monitorMemReader
monitorMemW width = graph 1 width monitorMemReader

monitorMemReader = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

graphCfg width colors = defaultGraphConfig { graphWidth = width
                                           , graphDataColors = colors
                                           , graphDirection = RIGHT_TO_LEFT
                                           }

graph delay width = pollingGraphNew (graphCfg width [(0x26/0xff, 0x8b/0xff, 0xd2/0xff, 1)]) delay
