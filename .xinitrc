resconfig
xconfig
BG_DIR="/media/home/wolke/.cache/backgrounds/fhd/"
( sleep 10; xconfig; killall bgset; bgset --exclude="porn/" "$BG_DIR" ) &

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
