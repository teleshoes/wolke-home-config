#!/usr/bin/perl
use strict;
use warnings;
use Cwd qw(abs_path);
use File::Basename qw(dirname);

my $A4_FREQ = 440;   #A440
my $INC = 2**(1/12); #equal temperament

my @NOTE_NAMES = qw(C Db D Eb E F Gb G Ab A Bb B);
my @OCTAVES = qw(3 4 5 6);

my $DURATION_S = 10;
my $SIN_DIR = abs_path(dirname($0));

sub main(@){
  my @notes = map {my $oct=$_; map {"$_$oct"} @NOTE_NAMES} @OCTAVES;
  my $a4Index = (grep {$notes[$_] eq "A4"} (0..$#notes))[0];

  my %freqs = map {$notes[$_] => $A4_FREQ/$INC**($a4Index-$_)} (0..$#notes);

  system "rm $SIN_DIR/sin-*.wav";
  system "rm $SIN_DIR/sin-*.flac";

  for my $note(@notes){
    my $freq = $freqs{$note};
    my $fileName = "$SIN_DIR/sin-$note.wav";
    system "ffmpeg",
      "-f", "lavfi",
      "-i", "sine=frequency=$freq:duration=$DURATION_S",
      $fileName;
    system "flac", "--best", $fileName;
  }

  system "rm $SIN_DIR/sin-*.wav";

  print "\n\n\n\n";

  for my $note(@notes){
    printf "%-3s , %.2f\n", $note, $freqs{$note};
  }
}

&main(@ARGV);
