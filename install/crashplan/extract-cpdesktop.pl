#!/usr/bin/perl
use strict;
use warnings;

my $defaultBinJar = '/usr/local/crashplan/lib/com.backup42.desktop.jar';
my $srcJar = "com.backup42.desktop.src.jar";
my $cpDesktopPath = "com/backup42/desktop/CPDesktop.java";

sub run(@);

sub main(@){
  my $binJar = shift if @_ > 0;
  $binJar = $defaultBinJar if not defined $binJar;
  die "Usage: $0 [JAR]   {default is $defaultBinJar}\n" if @_ > 0;
  die "jar does not exist: $binJar\n" if not -f $binJar;
  run "rm", "-f", $srcJar;
  run "./jd-cli", $binJar;
  die "source jar $srcJar does not exist\n" if not -f $srcJar;
  run "rm -rf src && mkdir src && cd src && unzip ../$srcJar";
  die "source file $cpDesktopPath does not exist\n" if not -f "src/$cpDesktopPath";
  run "cp", "src/$cpDesktopPath", ".";
  run "rm -rf $srcJar src/";
}

sub run(@){
  print "@_\n";
  system @_;
  die "Error running \"@_\"\n" if $? != 0;
}

&main(@ARGV);
