module TPBattStatApplet (main) where
import System.Process (system)
main = system "/usr/lib/tpbattstat-applet/tpbattstat.py --dzen 2500"
