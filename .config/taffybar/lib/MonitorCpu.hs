module MonitorCpu(monitorCpuW) where
import Color as C
import System.Information.CPU (cpuLoad)
import System.Information.Memory (parseMeminfo, memoryUsedRatio)
import System.Taffybar.Widgets.PollingGraph (
  pollingGraphNew, defaultGraphConfig, graphDataColors,
  graphDirection, GraphDirection(..))

graphCfg colors = defaultGraphConfig { graphDataColors = colors
                                     , graphDirection = RIGHT_TO_LEFT
                                     }

monitorCpuW = graph $ do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

graph = pollingGraphNew (graphCfg [rgba C.Green 1, rgba C.Purple 0.5]) 1
