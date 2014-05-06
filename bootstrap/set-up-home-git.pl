#!/usr/bin/perl
use strict;
use warnings;

sub run(@);

my $url = 'https://github.com/teleshoes/wolke-home-config.git';
my @dirs = (
  "install/root-files/usr/share/sounds/custom",
  ".config/taffybar/icons",
  "Desktop/Backgrounds",
);

sub main(@){
  my $host = `nuc`;
  chomp $host;

  run "nuc", "-u", "root", "apt-get install git rsync";
  run "nuc", "
    if [ ! -d .git ]; then
      git init
      git remote add origin \"$url\"
      git fetch
      rm .bash_logout .bashrc .profile .ssh/*.pub
      git checkout -t origin/nuc
    fi
  ";
  for my $dir(@dirs){
    run "rsync", "-avP", "$ENV{HOME}/$dir/", "$host:~/$dir/";
  }
}

sub run(@){
  print "@_\n";
  system @_;
}

&main(@ARGV);
