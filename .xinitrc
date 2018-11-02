resconfig
xconfig

sudo bluetooth off &
sudo fan default &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
/usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd &

( pulse-enable on ; pulse-vol mic mute ) &

MACHINE_TYPE=`cat ~/machine-type 2>/dev/null`

if [ $MACHINE_TYPE = "main" ]; then
  sudo wauto -p &
  term -t qtemail-daemon -r email-daemon.pl
  brightness 100 &
elif [ $MACHINE_TYPE = "aux" ]; then
  sudo wauto -p &
  brightness 100 &
elif [ $MACHINE_TYPE = "bed" ]; then
  sudo wauto -p &
  brightness 100 &
elif [ $MACHINE_TYPE = "tv" ]; then
  screensaver
  killall bgset
  bgset --exclude="porn/" &
  sudo fcron-start

  sudo igstart &
  cec-cmd start &
  ( sudo mnt
    sudo hdparm-tool --active /media/seedbox
    sudo hdparm-tool --log /media/videos >> hdparm-videos.log 2>>hdparm-videos.log
  ) &
  sudo wired &
  screen-daemon sb-daemon --start
  screen-daemon revtun-ensure --start
fi

exec xmonad
