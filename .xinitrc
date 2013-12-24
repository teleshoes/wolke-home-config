sudo net-mon stop
sudo wauto &

sudo mnt --other -m Charybdis

~/.xconfig
(sleep 10; xconfig) &

pulseaudio --start &
sudo bluetooth off &
sudo fan auto &
taffybar &

exec xmonad
