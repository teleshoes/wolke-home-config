Esetroot $HOME/Desktop/Backgrounds/venere-cropped.jpg
xsetroot -cursor_name left_ptr
for x in $HOME/.xresources-*; do xrdb -merge "$x"; done

setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option caps:escape

#tap to nipple click
#echo -n 1 | sudo tee /sys/devices/platform/i8042/serio1/serio2/press_to_select

tpDev='SynPS/2 Synaptics TouchPad'
xinput set-prop "$tpDev" "Synaptics Tap Time" 0
xinput set-prop "$tpDev" "Synaptics Two-Finger Scrolling" 0 0
xinput set-prop "$tpDev" "Synaptics Move Speed" 1 1.75 0.035842 40
xinput set-prop "$tpDev" "Synaptics Edge Scrolling" 0 0 0

#http://www.thinkwiki.org/wiki/How_to_configure_the_TrackPoint
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 8 6 7 4 5
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 8 200

xset s off
xset s noblank
xset dpms 0 0 0

xset -b #no bell

pulseaudio --start &
sudo wconnect `wguessid` &
sudo bluetooth off &
sudo fan auto
taffybar &

exec xmonad
