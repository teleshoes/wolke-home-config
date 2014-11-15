module MonitorCpu(monitorCpuW) where
import Color as C
import Utils (pollingGraphMain)
import System.Information.CPU (cpuLoad)
import System.Information.Memory (parseMeminfo, memoryUsedRatio)
import System.Taffybar.Widgets.PollingGraph (
  pollingGraphNew, defaultGraphConfig, graphDataColors,
  graphDirection, GraphDirection(..))

main = pollingGraphMain 1 monitorCpuReader
monitorCpuW = graph 1 monitorCpuReader

monitorCpuReader = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

graphCfg colors = defaultGraphConfig { graphDataColors = colors
                                     , graphDirection = RIGHT_TO_LEFT
                                     }

graph delay = pollingGraphNew (graphCfg [rgba C.Green 1, rgba C.Black 0.5]) delay
