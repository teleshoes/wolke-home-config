module MonitorMem(monitorMemW) where
import Color as C
import Utils (pollingGraphMain)
import System.Information.Memory (parseMeminfo, memoryUsedRatio)
import System.Taffybar.Widgets.PollingGraph (
  pollingGraphNew, defaultGraphConfig, graphDataColors,
  graphDirection, GraphDirection(..))

main = pollingGraphMain 1 monitorMemReader
monitorMemW = graph 1 monitorMemReader

monitorMemReader = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

graphCfg colors = defaultGraphConfig { graphDataColors = colors
                                     , graphDirection = RIGHT_TO_LEFT
                                     }

graph delay = pollingGraphNew (graphCfg [(0x26/0xff, 0x8b/0xff, 0xd2/0xff, 1)]) delay
