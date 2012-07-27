#!/usr/bin/perl
use strict;
use warnings;

my $kernel = `uname -r`;
chomp $kernel;

my $kernelMajorMinor = $1 if $kernel =~ /^(\d+\.\d+)/;
my $srcDir = $kernelMajorMinor < 3.3 ? '3.2' : '3.3';
print "installing version in $srcDir/\n";
chdir "$srcDir";
$ENV{PWD} = "$ENV{PWD}/$srcDir";

my $dir = "/lib/modules/$kernel/kernel/drivers/platform/x86/";
my $mod = "thinkpad_acpi.ko";

system "patch thinkpad_acpi.c led.patch";
system "make";
system "patch -R thinkpad_acpi.c led.patch";

if(-e $mod){
  print "\n\nsuccess!\n";
  my $now = `date +%s`;
  chomp $now;
  my $bak = "$mod.orig.$now";
  print "copying $mod to $dir\nbackup in $bak\n";
  system "sudo mv $dir/$mod $dir/$bak";
  system "sudo cp $mod $dir";
  print "remove and add module thinkpad_acpi\n";
  system "sudo modprobe -r thinkpad_acpi";
  system "sudo modprobe thinkpad_acpi";
}

print "Cleaning..\n";
system "make clean";
