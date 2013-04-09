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
          , netStatsW
          , netW
          , fcrondynW
          , klompW
          , volumeW
          , micW
          , pidginPipeW
          , thunderbirdW
          , ekigaW
          , cpuScalingW
          , cpuFreqsW
          , fanW
          , brightnessW
          , pingMonitorW "www.google.com" "G"
          , openvpnW
          , pingMonitorW "source.escribe.com" "E"
          , tpBattStatW
          , textClockNew Nothing "%a %b %d\n%H:%M:%S" 1
          ]

  defaultTaffybar defaultTaffybarConfig {
    barHeight=height, startWidgets=start, endWidgets=end}
