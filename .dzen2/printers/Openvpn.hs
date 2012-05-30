#!/usr/bin/perl
use strict;
use warnings;

my $LAUNCHERS = "\$HOME/.dzen2/launchers";
my $PRINTERS = "\$HOME/.dzen2/printers";

my $clickCmd = "sudo sslvpn toggle";

my $color;
my $onoff;
if(`pidof openvpn`){
  $color = "green";
  $onoff = "yes";
}else{
  $color = "red";
  $onoff = "off";
}

my $markup = `$PRINTERS/ghcprinter TextRows "vpn" "$onoff" 36`;
chomp $markup;
$markup = "^fg($color)$markup^fg()";
system "$PRINTERS/ghcprinter ClickAction \"$clickCmd\" \"$markup\"";
