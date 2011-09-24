#!/bin/bash
v=11-1
cd
sudo apt-get install \
  build-essential cdbs fakeroot dh-make debhelper \
  debconf libstdc++6 dkms libqtgui4 wget execstack libelfg0
sudo apt-get install ia32-libs
mkdir catalyst$v
cd catalyst$v
wget http://www2.ati.com/drivers/linux/ati-driver-installer-$v-x86.x86_64.run

