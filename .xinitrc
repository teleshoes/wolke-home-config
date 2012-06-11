Esetroot $HOME/Desktop/Backgrounds/venere-cropped.jpg
xsetroot -cursor_name left_ptr
xrdb $HOME/.Xresources

setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option caps:escape

#tap to nipple click
#echo -n 1 | sudo tee /sys/devices/platform/i8042/serio1/serio2/press_to_select

tpDev='SynPS/2 Synaptics TouchPad'
xinput set-prop "$tpDev" "Synaptics Tap Time" 0
xinput set-prop "$tpDev" "Synaptics Two-Finger Scrolling" 0 0
xinput set-prop "$tpDev" "Synaptics Move Speed" 1 1.75 0.035842 40
xinput set-prop "$tpDev" "Synaptics Edge Scrolling" 0 0 0

xset s off
xset s noblank
xset dpms 0 0 0

pulseaudio --start

sudo wconnect `wguessid` &

sudo hdmi-sound-card-remove

sudo bluetooth off

exec \
  dbus-launch --exit-with-session \
  ck-launch-session \
  xmonad
