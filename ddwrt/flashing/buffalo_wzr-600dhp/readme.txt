# only works with official buffalo firmware, apparently
# disconnect all networks, connect ethernet cable to laptop, unplug router

sudo ifconfig enp0s25 192.168.11.2
sudo arp -s 192.168.11.1 02:aa:bb:cc:dd:20
tftp

# tftp commands (last one runs every second for 60s until it works):

connect 192.168.11.1
binary
rexmt 1
timeout 60
trace
status
put official-buffalo-image.enc

# plug in router after running put, will say sent XXXXX bytes in however long

# plug in router
# on success, red light blinks every 1s for a few min, then router boots right up
