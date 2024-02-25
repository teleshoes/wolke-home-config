#!/bin/sh
DEV_DISK="/dev/nvme0n1"
DEV_EFI="/dev/nvme0n1p1"
DEV_ROOT="/dev/nvme0n1p2"

mount $DEV_ROOT -o subvol=@ --mkdir /mnt/root
mount $DEV_EFI              --mkdir /mnt/root/boot/efi

mount --bind  /dev     /mnt/root/dev
mount --bind  /dev/pts /mnt/root/dev/pts
mount --rbind /sys     /mnt/root/sys
mount --bind  /proc    /mnt/root/proc
mount --bind  /run     /mnt/root/run

chroot /mnt/root grub-install \
  --boot-directory=/boot \
  --efi-directory=/boot/efi \
  --target=x86_64-efi \
  --no-uefi-secure-boot \
  --bootloader-id=siduction \
  $DEV_DISK
