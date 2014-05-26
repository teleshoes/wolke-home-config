~/.xconfig
( sleep 10; xconfig ) &

sudo wauto &
pulseaudio --start &
sudo bluetooth off &
sudo fan auto &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
( sleep 2; pulse-vol mic mute ) &

exec xmonad
