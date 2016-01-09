module MonitorCpu(monitorCpuW) where
import Color as C
import Utils (pollingGraphMain)
import System.Information.CPU (cpuLoad)
import System.Information.Memory (parseMeminfo, memoryUsedRatio)
import System.Taffybar.Widgets.PollingGraph (
  pollingGraphNew, defaultGraphConfig, graphWidth, graphDataColors,
  graphDirection, GraphDirection(..))

main = pollingGraphMain 1 monitorCpuReader
monitorCpuW width = graph 1 width monitorCpuReader

monitorCpuReader = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

graphCfg width colors = defaultGraphConfig { graphWidth = width
                                           , graphDataColors = colors
                                           , graphDirection = RIGHT_TO_LEFT
                                           }

graph delay width = pollingGraphNew (graphCfg width [rgba C.Green 1, rgba C.Black 0.5]) delay
