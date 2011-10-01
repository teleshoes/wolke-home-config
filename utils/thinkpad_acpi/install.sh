#!/bin/bash
dir=/lib/modules/`uname -r`/kernel/drivers/platform/x86/
mod=thinkpad_acpi.ko
make
if [ -e thinkpad_acpi.ko]; then
  echo coping $mod to $dir, backing up orig as $mod.orig
  sudo mv $dir/$mod $dir/$mod.orig
  sudo cp $mod $dir
fi
