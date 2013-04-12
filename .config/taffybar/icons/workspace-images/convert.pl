#!/usr/bin/perl
use strict;
use warnings;

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
  my @sizes = ([16,16], [24,24], [36,36], [40,40], [48,48], [64,64]);
  for my $size(@sizes){
    my ($w, $h) = @$size;
    my $dir = "./${w}x${h}";
    run "rm", "-r", $dir;
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
