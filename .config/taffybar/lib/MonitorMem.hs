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

graph = pollingGraphNew (graphCfg [rgba C.Red 1]) 1
