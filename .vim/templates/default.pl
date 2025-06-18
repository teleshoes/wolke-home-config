#!/usr/bin/perl
use strict;
use warnings;
use File::Basename qw(basename);

my $EXEC = basename $0;

my $USAGE = "Usage:
  $EXEC -h|--help
    show this message
";

sub main(@){
  while(@_ > 0){
    my $arg = shift;
    if($arg =~ /^(-h|--help)$/){
      print $USAGE;
      exit 0;
    }else{
      die "ERROR: unknown arg $arg\n";
    }
  }
}

&main(@ARGV);
