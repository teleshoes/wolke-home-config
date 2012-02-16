#!/bin/bash
dir=/lib/modules/`uname -r`/kernel/drivers/platform/x86/
mod=thinkpad_acpi.ko

patch thinkpad_acpi.c led.patch

make

patch -R thinkpad_acpi.c led.patch

if [ -e thinkpad_acpi.ko ]; then
  echo coping $mod to $dir, backing up orig as $mod.orig
  sudo mv $dir/$mod $dir/$mod.orig.`date +%s`
  sudo cp $mod $dir
fi
make clean
