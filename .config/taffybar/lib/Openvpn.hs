module Openvpn(openvpnW) where
import Widgets (clickableLeft)
import Utils (isRunning, fg)

clickCmd = "sudo sslvpn toggle"

vpnMarkup = do
  vpnOn <- isRunning "openvpn"
  let text = if vpnOn then "yes" else "off"
  let color = if vpnOn then "green" else "red"
  return $ fg color $ "vpn\n" ++ text

openvpnW w = do
  lbl <- w vpnMarkup
  click <- clickableLeft lbl clickCmd
  return click
