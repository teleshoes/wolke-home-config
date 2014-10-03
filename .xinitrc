xconfig
BG_DIR="/media/home/wolke/.cache/backgrounds/fhd/"
( sleep 10; xconfig; killall bgset; bgset --exclude="porn/" "$BG_DIR" ) &

screen-cmd fetch-seedbox

brightness 80
sudo mnt &
sudo wired &
execPing screen-cmd revtun &
pulseaudio --start &
sudo bluetooth off &
sudo fan auto &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
( sleep 2; pulse-vol mic mute ) &

exec xmonad
