#!/usr/bin/perl
use strict;
use warnings;

my $PIPEFILE = `echo -n \$HOME/.purple/plugins/pipe`;
my $PRINTERS = `echo -n \$HOME/.dzen2/printers`;
my $HBIN = `echo -n \$HOME/bin`;

my $height = shift;
$height = 24 if not defined $height;
my $IMGDIR = `echo -n \$HOME/.dzen2/icons/${height}x${height}/pidgin`;

sub img($);


my $runningLeftCmd = "xdotool key --clearmodifiers alt+2";
my $notRunningLeftCmd = "pidgin";
my $leftCmd =
  "pidof pidgin; " .
  "if [ \$? == 0 ]; " .
  "then $runningLeftCmd; " .
  "else $notRunningLeftCmd; " .
  "fi";

my $middleCmd = "killall pidgin; pidgin";
my $rightCmd = "killall pidgin";

my %imgMarkupPerStatus = (
  'off'            => img 'not-running.xpm',
  'new message'    => img 'pidgin-tray-pending.xpm',

  'available'      => img 'pidgin-tray-available.xpm',
  'away'           => img 'pidgin-tray-away.xpm',
  'do not disturb' => img 'pidgin-tray-busy.xpm',
  'invisible'      => img 'pidgin-tray-invisible.xpm',
  'offline'        => img 'pidgin-tray-offline.xpm',
);
my $defaultImgMarkup = img 'pidgin-tray-xa.xpm';


sub main(){
  my $status = `cat $PIPEFILE`;
  chomp $status;
  $status = lc $status;
  if($status ne 'off'){
    system "pidof pidgin > /dev/null";
    if($? != 0){
      $status = 'off';
    }
  }

  my $imgMarkup = $imgMarkupPerStatus{$status};
  $imgMarkup = $defaultImgMarkup if not defined $imgMarkup;

  system "$PRINTERS/ghcprinter", "ClickableImage", $leftCmd, $middleCmd, $rightCmd, $imgMarkup;
}

sub img($){
  my $img = shift;
  return "$IMGDIR/$img";
}

main;
