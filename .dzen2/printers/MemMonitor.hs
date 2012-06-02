#!/usr/bin/perl
use strict;
use warnings;

my $delay = 1.5;
my $DIR = "\$HOME/.dzen2/printers";
my @colors = map {"\\#$_"} qw(00b25b 00e575 00fe81 a9f4c4 000000);
exec "$DIR/mem-percentages $delay | $DIR/ghcprinter PercentMonitor @colors";
