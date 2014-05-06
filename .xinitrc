~/.xconfig
(sleep 10; xconfig) &

mnt
sudo wired &
pulseaudio --start &
sudo bluetooth off &
sudo fan auto &
taffybar &
parcellite -d &
( sleep 2; pulse-vol mic mute ) &

exec xmonad
