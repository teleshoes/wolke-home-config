#!/usr/bin/perl
use strict;
use warnings;

my $zeniusFile = "$ENV{HOME}/.zenius";

my %knownIds;
foreach my $line(`cat $zeniusFile 2>/dev/null`){
  if($line =~ /^(\d+) \| (.*)\n$/){
    $knownIds{$1} = $2;
  }
}

my $url = ''
  . "http://zenius-i-vanisher.com"
  . "/v5.2"
  . "/simfiles.php"
  . "?category=latest20user";

my $html = `wget -q -O - $url`;

my @entries;
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
    push @entries, [$1, $2, $4, $5];
}

foreach my $entry(@entries){
  my ($id, $name, $category, $relDate) = @$entry;
  my $secAgo = -1;
  if($relDate =~ /^([0-9.]+) hours ago$/){
    $secAgo = int($1*60*60);
  }elsif($relDate =~ /^([0-9.]+) days ago$/){
    $secAgo = int($1*24*60*60);
  }
  my $date = `date --date="$secAgo seconds ago"`;
  chomp $date;
  if($? != 0){
    $date = `date`;
    chomp $date;
    $date .= "{$relDate}";
  }
  if(not defined $knownIds{$id}){
    my $str = "$id | $name | $category | $date";
    system "echo \"$str\" >> $zeniusFile";
    print "$str\n";
  }
}

