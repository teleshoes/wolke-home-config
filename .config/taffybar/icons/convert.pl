#!/usr/bin/perl
use strict;
use warnings;

my @heights = (8, 10, 12, 16, 18, 20, 24, 28, 30, 36, 38, 40, 48, 50, 64);

sub convertRsvg($$$);
sub convertImageMagick($$$);
sub run(@);

sub main(@){
  my @imgs = `cd scalable; find -name '*.svg' -or -name '*.png'`;

  my @sizeDirs = `find -mindepth 1 -maxdepth 1 -regex '.*/[0-9]+'`;
  for my $sizeDir(@sizeDirs){
    chomp $sizeDir;
    run "rm -r $sizeDir/";
  }

  for my $h(@heights){
    my $dir = "./$h";
    for my $img(@imgs){
      chomp $img;
      my $dest = $img;
      $dest =~ s/\.[a-zA-Z0-9]+$/.png/i;

      my $destDir = "$dir/$dest";
      $destDir =~ s/\/[^\/]*$//;
      run "mkdir", "-p", $destDir;

      if($img =~ /\.svg$/i){
        convertRsvg "scalable/$img", "$dir/$dest", $h;
      }else{
        convertImageMagick "scalable/$img", "$dir/$dest", $h;
      }
    }
  }
}

sub convertRsvg($$$){
  my ($src, $dest, $h) = @_;
  if(`which rsvg-convert` =~ /rsvg-convert/){
    run "rsvg-convert",
      "-h", $h,
      "-a",
      "-f", "png",
      "-o", $dest,
      $src;
  }elsif(`which rsvg` =~ /rsvg/){
    run "rsvg",
      "-h", $h,
      "-a",
      "-f", "png",
      "-o", $dest,
      $src, "$dest";
  }
}
sub convertImageMagick($$$){
  my ($src, $dest, $h) = @_;
  run "convert", "-resize", "${h}x${h}", $src, $dest;
}

sub run(@){
  print "@_\n";
  system @_;
}

&main(@ARGV);
