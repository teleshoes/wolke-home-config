~/.xconfig
(sleep 10; xconfig) &

sudo wauto &
pulseaudio --start &
sudo bluetooth off &
sudo fan auto &
taffybar &
parcellite -d &
pulse-vol mic mute

exec xmonad
