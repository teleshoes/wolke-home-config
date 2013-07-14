#!/usr/bin/perl
use strict;
use warnings;

my $fstab = '# /etc/fstab: static file system information.
# <file system> <mount point>   <type>  <options>       <dump>  <pass>
proc        /proc           proc    nodev,noexec,nosuid 0 0
tmpfs       /tmp            tmpfs   defaults,noatime,nodev,nosuid,mode=1777 0 0
LABEL=root  /               ext4    errors=remount-ro 0 1
LABEL=home  /home           ext4    defaults 0 2
';

my $fstabFile = '/etc/fstab';

print "OLD:\n";
system "cat $fstabFile";
print "\n---\n";
print "NEW:\n";
print $fstab;

open FH, "| sudo tee $fstabFile > /dev/null";
print FH $fstab;
close FH;
