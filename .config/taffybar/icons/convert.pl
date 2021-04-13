#!/usr/bin/perl
use strict;
use warnings;
use File::Basename qw(basename dirname);

my @HEIGHTS = (8, 10, 12, 16, 18, 20, 24, 28, 30, 36, 38, 40, 48, 50, 64);

my $EXEC = basename $0;
my $BASE_DIR = dirname $0;
my $SCALABLE_DIR = "$BASE_DIR/scalable";

my $OK_SVG_EXTS = join "|", qw(svg);
my $OK_OTHER_EXTS = join "|", qw(png jpg jpeg bmp xpm);
my $OK_ALL_EXTS = "$OK_SVG_EXTS|$OK_OTHER_EXTS";

my $USAGE = "Usage:
  $EXEC -h|--help
    show this message

  $EXEC [OPTS]
    convert icons in $SCALABLE_DIR
      to each dirs: $BASE_DIR/<HEIGHT>
      using rsvg + imageMagick
    skip icons that already exist

  HEIGHT
    one of: @HEIGHTS

  OPTS
    -r | --clean | --reset | --init
      before converting, recursively remove all dirs that look like:
        $BASE_DIR/<INT>/
        (where <INT> is any integer)
";

sub getScalableImages();
sub getSizeDirs();
sub convertRsvg($$$);
sub convertImageMagick($$$);
sub run(@);

sub main(@){
  my $clean = 0;
  while(@_ > 0){
    my $arg = shift @_;
    if($arg =~ /^(-h|--help)$/){
      print $USAGE;
      exit 0;
    }elsif($arg =~ /^(-r|--clean|--reset|--init)$/){
      $clean = 1;
    }else{
      die $USAGE;
    }
  }

  my @scalableImages = getScalableImages();

  my @sizeDirs = getSizeDirs();
  for my $sizeDir(@sizeDirs){
    run "rm -r $sizeDir/" if $clean;
  }

  for my $h(@HEIGHTS){
    my $sizeDir = "$BASE_DIR/$h";
    for my $srcImg(@scalableImages){
      my $destImg = $srcImg;
      $destImg =~ s/\.[a-zA-Z0-9]+$/.png/i;

      my $srcImgFile = "$SCALABLE_DIR/$srcImg";
      my $destImgFile = "$sizeDir/$destImg";

      next if -e $destImgFile;

      my $destDir = dirname "$sizeDir/$destImg";
      run "mkdir", "-p", $destDir;

      if($srcImg =~ /\.($OK_SVG_EXTS)$/i){
        convertRsvg $srcImgFile, $destImgFile, $h;
      }elsif($srcImg =~ /\.($OK_OTHER_EXTS)$/i){
        convertImageMagick $srcImgFile, $destImgFile, $h;
      }else{
        die "ERROR: img $srcImg must end in $OK_ALL_EXTS\n";
      }
    }
  }
}

sub getScalableImages(){
  my @scalableImages = `find $SCALABLE_DIR/ -type f`;
  chomp foreach @scalableImages;
  for my $img(@scalableImages){
    if($img !~ s/^$SCALABLE_DIR\///){
      die "ERROR: malformed `find` output: $img\n";
    }
    if($img !~ /\.($OK_ALL_EXTS)$/){
      die "ERROR: img $img must end in $OK_ALL_EXTS\n";
    }
  }
  return @scalableImages;
}

sub getSizeDirs(){
  my @sizeDirs = glob("$BASE_DIR/*");
  return grep {-d $_ and $_ =~ /\/\d+$/} @sizeDirs;
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
  if($? != 0){
    die "ERROR: \"@_\" exited with non-zero exit code\n";
  }
}

&main(@ARGV);
