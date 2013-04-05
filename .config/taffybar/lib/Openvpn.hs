module Openvpn(openvpnW) where
import Utils (isRunning, fg)
import TextRows (textRows)
import ClickAction (clickAction)

clickCmd = "sudo sslvpn toggle"

openvpnW w = w $ do
  vpnOn <- isRunning "openvpn"
  let text = if vpnOn then "yes" else "off"
  let color = if vpnOn then "green" else "red"
  return $ clickAction 1 clickCmd $ fg color $ textRows "vpn" text
