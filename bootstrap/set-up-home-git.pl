#!/usr/bin/perl
use strict;
use warnings;

my $url = 'https://github.com/teleshoes/wolke-home-config.git';
my @dirs = (
  "install/root-files/usr/share/sounds/custom",
);

sub main(@){
  my $host = `nuc`;
  chomp $host;

  system "nuc", "-u", "root", "apt-get install git rsync";
  system "nuc", "
    if [ ! -d .git ]; then
      git init
      git remote add origin \"$url\"
      git fetch
      rm .bash_logout .bashrc .profile .ssh/*.pub
      git checkout -t origin/nuc
    fi
  ";
  for my $dir(@dirs){
    system "rsync", "-avP", "$ENV{HOME}/$dir/", "$host:~/$dir/";
  }
}

&main(@ARGV);
