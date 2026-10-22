#!/usr/bin/perl
use strict;
use warnings;
use File::Basename qw(basename);

my $EXEC = basename $0;

my $USAGE = "Usage:
  $EXEC -h|--help
    show this message

  $EXEC FILE
    parse KML file with <coordinates> and print for each tag:
      POLY_<NUM>:
          latLonPoint: --
          latLonPolygon: ['<LAT>,<LON>', '<LAT>,<LON>'...]
";

sub main(@){
  my $file = undef;
  while(@_ > 0){
    my $arg = shift;
    if($arg =~ /^(-h|--help)$/){
      print $USAGE;
      exit 0;
    }elsif(-f $arg){
      $file = $arg;
    }else{
      die "ERROR: unknown arg $arg\n";
    }
  }
  die "$USAGE\nERROR: missing FILE\n" if not defined $file;

  open my $fh, "< $file" or die "ERROR: could not read $file\n$!\n";
  my @lines = <$fh>;
  close $fh;

  my $idx = 1;
  for my $line(@lines){
    if($line =~ /<coordinates>(.*)<\/coordinates>/){
      my $coordStr = $1;
      my @coords = split /\s+/, $coordStr;
      my @latLonStrs;
      for my $coord(@coords){
        next if $coord =~ /^\s*$/;
        if($coord !~ /^(-?\d+|-?\d*\.\d+),(-?\d+|-?\d*\.\d+)$/){
          die "ERROR: invalid coord $coord\n";
        }
        my ($lon, $lat) = ($1, $2);
        push @latLonStrs, sprintf("%.6f,%.6f", $lat, $lon);
      }
      my $csv = join ", ", map {"'$_'"} @latLonStrs;
      printf "  POLY_%02d\n    latLonPoint: --\n    latLonPolygon: [%s]\n", $idx, $csv;
      $idx++;
    }
  }
}

&main(@ARGV);
