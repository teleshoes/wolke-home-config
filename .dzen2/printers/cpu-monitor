#!/usr/bin/perl
use strict;
use warnings;

my $delay = 1.5;
my $DIR = "$ENV{HOME}/.dzen2/printers";
my @colors = map {"\\#$_"} qw(0072b2 0091e5 00a2fe 002f3d 000000);
exec "$DIR/cpu-percentages $delay | $DIR/ghcprinter PercentMonitor @colors";
