#!/usr/bin/perl
use strict;
use warnings;

my $zeniusFile = "$ENV{HOME}/.zenius";

sub fetchNewSimfiles();
sub getKnownSimfiles();
sub getLatestUserSimfilesPage();
sub parseSimfilesPage($);
sub getAbsoluteDate($);
sub entryToString($);

sub main(@){
  fetchNewSimfiles;
}

sub fetchNewSimfiles(){
  my $html = getLatestUserSimfilesPage;
  my @entries = parseSimfilesPage $html;
  my %knownSimfiles = getKnownSimfiles;
  foreach my $entry(@entries){
    my $id = ${$entry}[0];
    if(not defined $knownSimfiles{$id}){
      my $str = entrytoString $entry;
      system "echo \"$str\" >> $zeniusFile";
      print "$str\n";
    }
  }
}

sub getKnownSimfiles(){
  my %knownSimfiles;
  foreach my $line(`cat $zeniusFile 2>/dev/null`){
    if($line =~ /^(\d+) \| (.*)\n$/){
      $knownSimfiles{$1} = $2;
    }
  }
  return %knownSimfiles;
}

sub getLatestUserSimfilesPage(){
  my $url = ''
    . "http://zenius-i-vanisher.com"
    . "/v5.2"
    . "/simfiles.php"
    . "?category=latest20user"
    ;
  return `wget -q -O - $url`;
}

sub parseSimfilesPage($){
  my $html = shift;
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
  return @entries;
}

sub getAbsoluteDate($){
  my $relDate = shift;
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
  return $date;
}

sub entryToString($){
  my ($id, $name, $category, $relDate) = @{shift()};
  my $date = getAbsoluteDate $relDate;
  return "$id | $name | $category | $date";
}

&main(@ARGV);
