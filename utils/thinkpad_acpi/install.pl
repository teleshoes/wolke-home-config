#!/bin/bash
dir=/lib/modules/`uname -r`/kernel/drivers/platform/x86/
mod=thinkpad_acpi.ko

patch thinkpad_acpi.c led.patch

make

patch -R thinkpad_acpi.c led.patch

if [ -e thinkpad_acpi.ko ]; then
  echo; echo; echo success!
  bak="$mod.orig.`date +%s`"
  echo copying $mod to $dir
  echo backup in $bak
  sudo mv $dir/$mod $dir/$bak
  sudo cp $mod $dir
  echo modprobe -r thinkpad_acpi, then modprobe thinkpad_acpi
  sudo modprobe -r thinkpad_acpi
  sudo modprobe thinkpad_acpi
fi
echo
echo Cleaning..
make clean
