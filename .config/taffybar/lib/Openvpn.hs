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
  clickableLeft (sslCmdText "toggle" confName) label

sslCmd :: String -> String -> [String]
sslCmd cmd confName = ["sudo", "sslvpn", cmd, confName]
sslCmdText :: String -> String -> String
sslCmdText cmd confName = intercalate " " $ sslCmd cmd confName

openvpnReader confName display = do
  vpnOn <- procSuccess $ sslCmd "running" confName
  let color = if vpnOn then "green" else "red"
  return $ fg color $ display
