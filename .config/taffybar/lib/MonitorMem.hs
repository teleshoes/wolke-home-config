module MonitorMem(monitorMemW) where
import Color as C
import System.Information.Memory (parseMeminfo, memoryUsedRatio)
import System.Taffybar.Widgets.PollingGraph (
  pollingGraphNew, defaultGraphConfig, graphDataColors,
  graphDirection, GraphDirection(..))

graphCfg colors = defaultGraphConfig { graphDataColors = colors
                                     , graphDirection = RIGHT_TO_LEFT
                                     }

monitorMemW = graph $ do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

graph = pollingGraphNew (graphCfg [(0x26/0xff, 0x8b/0xff, 0xd2/0xff, 1)]) 1
