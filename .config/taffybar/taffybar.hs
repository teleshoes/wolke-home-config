import Brightness(brightnessW)
import Clock(clockW)
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
import Sep(sepW)
import TPBattStat(tpBattStatW)
import Thunderbird(thunderbirdW)
import Volume(volumeW)
import WMLog(wmLogNew)

import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, widgetSpacing, startWidgets, endWidgets)

main = do
  let cfg = defaultTaffybarConfig {barHeight=36, widgetSpacing=5}

  let start = [ wmLogNew ]
  let end = reverse
          [ monitorCpuW
          , monitorMemW
          , netStatsW
          , sepW
          , netW
          , sepW
          , fcrondynW
          , sepW
          , klompW
          , volumeW
          , micW
          , pidginPipeW $ barHeight cfg
          , thunderbirdW $ barHeight cfg
          , ekigaW
          , cpuScalingW
          , cpuFreqsW
          , fanW
          , brightnessW
          , pingMonitorW "www.google.com" "G"
          , openvpnW
          , pingMonitorW "source.escribe.com" "E"
          , tpBattStatW
          , sepW
          , clockW
          ]

  defaultTaffybar cfg {startWidgets=start, endWidgets=end}
