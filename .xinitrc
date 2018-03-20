~/.xconfig
( sleep 10; xconfig ) &

sudo wauto -p &
pulse-enable on
sudo bluetooth off &
sudo fan 1 &
taffybar-restart --daemon --quiet
parcellite -d &
sudo sbox-umount &
( sleep 2; pulse-vol mic mute ) &
term -t qtemail-daemon -r email-daemon.pl
term -t charge-limit-daemon -r charge-limit-daemon

/usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd &

exec xmonad
