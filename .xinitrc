resconfig
~/.xconfig
( sleep 10; xconfig ) &

brightness 80
sudo mnt &
sudo wired &
pulse-enable on
sudo bluetooth off &
sudo fan default &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
( sleep 2; pulse-vol mic mute ) &
term -t qtemail-daemon -r email-daemon.pl

/usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd &

exec xmonad
