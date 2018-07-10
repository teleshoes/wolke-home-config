module Openvpn(openvpnW) where
import Clickable (clickableLeft)
import Label (labelW, mainLabel)
import Utils (procSuccess, fg)
import Data.List (intercalate)

import System.Environment (getArgs)
import Control.Monad (when)

main = do
  args <- getArgs
  when (length args /= 1) (error "Usage: Openvpn CONF_NAME")
  let confName = args !! 0
  mainLabel $ openvpnReader confName $ take 3 confName
openvpnW confName display = do
  label <- labelW $ openvpnReader confName display
  clickableLeft (vpnCmdText "toggle" confName) label

vpnCmd :: String -> String -> [String]
vpnCmd cmd confName = ["sudo", "vpn", cmd, confName]
vpnCmdText :: String -> String -> String
vpnCmdText cmd confName = intercalate " " $ vpnCmd cmd confName

openvpnReader confName display = do
  vpnOn <- procSuccess $ vpnCmd "running" confName
  let color = if vpnOn then "green" else "red"
  return $ fg color $ display
