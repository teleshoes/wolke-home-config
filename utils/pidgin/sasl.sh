#!/bin/sh
d=/usr/lib/purple-2
sudo mv $d/libirc.so $d/libirc.so-bak_`date +%s`
sudo cp libirc.so $d
