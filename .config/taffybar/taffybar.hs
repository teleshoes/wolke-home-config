import Brightness(brightnessW)
import CpuFreqs(cpuFreqsW)
import CpuScaling(cpuScalingW)
import Ekiga(ekigaW)
import Fan(fanW)
import Fcrondyn(fcrondynW)
import Klomp(klompW)
import Mic(micW)
import MonitorCpu(monitorCpuW)
import MonitorMem(monitorMemW)
import Net(netW)
import NetStats(netStatsW)
import Openvpn(openvpnW)
import PidginPipe(pidginPipeW)
import PingMonitor(pingMonitorW)
import TPBattStat(tpBattStatW)
import Thunderbird(thunderbirdW)
import Volume(volumeW)
import Widgets(label)

import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, startWidgets, endWidgets)
import System.Taffybar.XMonadLog (xmonadLogNew)
import System.Taffybar.SimpleClock (textClockNew)

main = do
  let height = 40
  let start =
            [ xmonadLogNew
            ]
  let end = reverse
          [ monitorCpuW
          , monitorMemW
          , netStatsW $ label 1
          , netW $ label 1
          , fcrondynW $ label 1
          , klompW $ label 1
          , volumeW
          , micW
          , pidginPipeW
          , thunderbirdW
          , ekigaW
          , cpuScalingW $ label 1
          , cpuFreqsW $ label 1
          , fanW $ label 1
          , brightnessW
          , pingMonitorW (label 1) "www.google.com" "G" 1
          , openvpnW $ label 1
          , pingMonitorW (label 1) "source.escribe.com" "E" 1
          , tpBattStatW
          , textClockNew Nothing "%a %b %d\n%H:%M:%S" 1
          ]

  defaultTaffybar defaultTaffybarConfig {
    barHeight=height, startWidgets=start, endWidgets=end}
