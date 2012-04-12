#!/usr/bin/perl
use strict;
use warnings;

my $zeniusFile = "$ENV{HOME}/.zenius";

my %knownIds;
foreach my $line(`cat $zeniusFile`){
  if($line =~ /^(\d+): (.*)\n$/){
    $knownIds{$1} = $2;
  }
}

my $url = ''
  . "http://zenius-i-vanisher.com"
  . "/v5.2"
  . "/simfiles.php"
  . "?category=latest20user";

my $html = `wget -q -O - $url`;

while($html =~ /
  <a \s* href="viewsimfile\.php\?simfileid=(\d+)">
     ([^<]+)
  <\/a>
  .*?
  <a \s* href="viewsimfilecategory\.php\?categoryid=(\d+)">
    ([^<]*)
  <\/a>
  .*?
  <span[^>]*>([^<]*)<\/span>
  /gsxi){
  if(not defined $knownIds{$1}){
    system "echo \"$1: $2 - $4\" >> $zeniusFile";
    print "$2 - $4 - $5\n";
  }
}

