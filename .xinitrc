resconfig
xconfig

sudo bluetooth off &
sudo fan default &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
/usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd &

MACHINE_TYPE=`cat ~/machine-type 2>/dev/null`

if [ $MACHINE_TYPE = "main" ]; then
  sudo wauto -p &
  ( pulse-enable on ; pulse-vol mic mute ) &
  term -t qtemail-daemon -r email-daemon.pl
  brightness 100 &
elif [ $MACHINE_TYPE = "aux" ]; then
  sudo wauto -p &
  ( pulse-enable on ; pulse-vol mic mute ) &
  brightness 100 &
elif [ $MACHINE_TYPE = "bed" ]; then
  sudo wauto -p &
  ( pulse-enable on ; pulse-vol mic mute ) &
  brightness 100 &
elif [ $MACHINE_TYPE = "tv" ]; then
  ( pulse-enable on ; pulse-vol mic mute ) &
  screensaver
  killall bgset
  bgset --exclude="porn/" &
  sudo fcron-start

  sudo igstart &
  cec-cmd start &
  sudo mnt &
  sudo wired &
  screen-daemon sb-daemon --start
  screen-daemon revtun-ensure --start
  screen-daemon silence-daemon --start
fi

exec xmonad
