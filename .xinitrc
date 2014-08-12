xconfig
BG_DIR="/media/home/wolke/.cache/backgrounds/fhd/"
killall wallpaper-changer
( sleep 10; xconfig; wallpaper-changer --exclude="porn/" "$BG_DIR" ) &

brightness 80
sudo mnt &
sudo wired &
pulseaudio --start &
sudo bluetooth off &
sudo fan auto &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
( sleep 2; pulse-vol mic mute ) &

exec xmonad
