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
import TPBattStat(tpBattStatW)
import Thunderbird(thunderbirdW)
import Volume(volumeW)
import WMLog(wmLogNew)

import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, startWidgets, endWidgets)

main = do
  let height = 40
  let start = [ wmLogNew ]
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
          , clockW
          ]

  defaultTaffybar defaultTaffybarConfig {
    barHeight=height, startWidgets=start, endWidgets=end}
