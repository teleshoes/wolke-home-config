#!/usr/bin/perl
use strict;
use warnings;

sub run(@){
  print "@_\n";
  system @_;
}

sub main(@){
  die "Usage: CUE_FILE FLAC_FILE\n" if @_ != 2;
  run "cuebreakpoints \"$_[0]\" | shnsplit -o flac \"$_[1]\"";
}

&main(@ARGV);
