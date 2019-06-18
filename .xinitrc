resconfig --reset-res
resconfig --apply
xconfig

sudo display-manager-tool --check

sudo bluetooth off &
sudo fan default &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
/usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd &

MACHINE_TYPE=`cat ~/machine-type 2>/dev/null`

if [ $MACHINE_TYPE = "main" ]; then
  sudo wauto -p &
  brightness 100 &
  ( pulse-enable on ; pulse-vol mic mute ) &

  term -t qtemail-daemon -r email-daemon.pl
elif [ $MACHINE_TYPE = "aux" ]; then
  sudo wauto -p &
  brightness 100 &
  ( pulse-enable on ; pulse-vol mic mute ) &
elif [ $MACHINE_TYPE = "bed" ]; then
  sudo wauto -p &
  brightness 100 &
  ( pulse-enable on ; pulse-vol mic mute ) &
elif [ $MACHINE_TYPE = "tv" ]; then
  sudo wired &

  (
    pulse-enable on ;
    sleep 1 ;
    pulse-enable on ;
    sleep 5 ;
    pulse-enable on ;
    sleep 1 ;

    speaker -s hdmi surround ;
    pulse-vol mic mute ;
    screen-daemon silence-daemon --start ;
  ) &

  ( set -x ;
    date ;
    echo mount attempt 1/3 ;
    sudo mnt ;
    sleep 5 ;

    date ;
    echo mount attempt 2/3 ;
    sudo mnt ;
    sleep 5 ;

    date ;
    echo mount attempt 3/3 ;
    sudo mnt ;
    echo done ;

    date ;
  ) 2>&1 > /tmp/mnt-log &

  screensaver
  killall bgset
  bgset --exclude="porn/" &

  sudo fcron-start

  sudo igstart &
  cec-cmd start &

  screen-daemon sb-daemon --start
  screen-daemon revtun-ensure --start
fi

exec xmonad
