module Openvpn(main) where
import Utils (isRunning, fg)
import TextRows (textRows)
import ClickAction (clickAction)

height = 36
clickCmd = "sudo sslvpn toggle"

main = do
  vpnOn <- isRunning "openvpn"
  let text = if vpnOn then "yes" else "off"
  let color = if vpnOn then "green" else "red"
  putStrLn $ clickAction "1" clickCmd $ fg color $ textRows "vpn" text height
