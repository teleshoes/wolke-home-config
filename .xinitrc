xconfig
BG_DIR="/media/home/wolke/.cache/backgrounds/fhd/"
( sleep 10; xconfig; killall bgset; bgset --exclude="porn/" "$BG_DIR" ) &

screen-cmd fetch-seedbox

screen-cmd execPing --watchdog ddwrt --restart-dev

brightness 80
cec-cmd start &
( sudo mnt
  sudo hdparm-tool --idle /media/videos
  sudo hdparm-tool --log /media/videos >> hdparm-videos.log 2>>hdparm-videos.log
) &
sudo wired &
execPing screen-cmd revtun-ensure &
pulse-enable on
sudo bluetooth off &
sudo fan auto &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
( sleep 2; pulse-vol mic mute ) &

exec xmonad
