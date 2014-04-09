module Openvpn(openvpnW) where
import Clickable (clickableLeft)
import Label (labelW)
import Utils (procSuccess, fg)
import Data.List (intercalate)

sslCmd :: String -> String -> [String]
sslCmd cmd confName = ["sudo", "sslvpn", cmd, confName]
sslCmdText :: String -> String -> String
sslCmdText cmd confName = intercalate " " $ sslCmd cmd confName

vpnMarkup confName = do
  vpnOn <- procSuccess $ sslCmd "running" confName
  let text = if vpnOn then "yes" else "off"
  let color = if vpnOn then "green" else "red"
  return $ fg color $ take 3 confName ++ "\n" ++ text

openvpnW confName = do
  label <- labelW $ vpnMarkup confName
  clickableLeft (sslCmdText "toggle" confName) label
