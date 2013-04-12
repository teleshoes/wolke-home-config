#!/usr/bin/perl
use strict;
use warnings;

my @sizes = ([18,18], [24,24], [36,36], [40,40], [48,48], [64,64]);

sub run(@){
  print "@_\n";
  system @_;
}

sub convertRsvg($$$$){
  my ($src, $dest, $w, $h) = @_;
  run "rsvg",
    "-w", $w, "-h", $h,
    "-f", "png",
    "-o", $dest,
    $src, $dest;
}
sub convertImageMagick($$$$){
  my ($src, $dest, $w, $h) = @_;
  run "convert",
    "-resize", "${w}x${h}",
    "-gravity", "center",
    "-extent", "${w}x${h}",
    $src, $dest;
}

sub main(@){
  my @imgs = `ls source/`;

  my @sizeDirs = `find -mindepth 1 -maxdepth 1 -regex '.*/[0-9]+x[0-9]+'`;
  for my $sizeDir(@sizeDirs){
    chomp $sizeDir;
    run "rm -r $sizeDir/";
  }

  for my $size(@sizes){
    my ($w, $h) = @$size;
    my $dir = "./${w}x${h}";
    run "mkdir", $dir;
    for my $img(@imgs){
      chomp $img;
      my $dest = $img;
      $dest =~ s/\.[a-zA-Z0-9]+$/.png/i;

      if($img =~ /\.svg$/i){
        convertRsvg "source/$img", "$dir/$dest", $w, $h;
      }else{
        convertImageMagick "source/$img", "$dir/$dest", $w, $h;
      }
    }
  }
}

&main(@ARGV);
