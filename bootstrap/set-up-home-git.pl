#!/usr/bin/perl
use strict;
use warnings;

my $url = 'https://github.com/teleshoes/wolke-home-config.git';

sub main(@){
  system "nuc", "-u", "root", "apt-get install git";
  system "nuc", "
    if [ ! -d .git ]; then
      git init
      git remote add origin \"$url\"
      git fetch
      rm .bash_logout .bashrc .profile .ssh/*.pub
      git checkout -t origin/nuc
    fi
  ";
}

&main(@ARGV);
