~/.xconfig
(sleep 10; xconfig) &

sudo wauto &
pulseaudio --start &
sudo bluetooth off &
sudo fan auto &
taffybar-restart --daemon
parcellite -d &
( sleep 2; pulse-vol mic mute ) &

exec xmonad
