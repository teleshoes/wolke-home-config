#!/usr/bin/perl
use strict;
use warnings;

my $usage = "Usage:
  $0               - fetch the latest simfiles, print new ones
  $0 print         - print all known simfiles, newest first
  $0 <simfileid>   - wget the main simfile file
";

my $zeniusFile = "$ENV{HOME}/.zenius";

sub fetchNewSimfiles();
sub printKnownSimfiles();
sub fetchSimfile($);
sub getKnownSimfiles();
sub getLatestUserSimfilesPage();
sub parseSimfilesPage($);
sub getAbsoluteDate($);
sub entryEpoch($);
sub entryToString($);
sub entryFromString($);

sub main(@){
  my $arg = shift;
  if(not defined $arg){
    fetchNewSimfiles;
  }elsif($arg eq 'print'){
    printKnownSimfiles;
  }elsif($arg =~ /^\d+$/){
    fetchSimfile $arg;
  }else{
    die $usage;
  }
}

sub fetchNewSimfiles(){
  my $html = getLatestUserSimfilesPage;
  my @entries = parseSimfilesPage $html;
  my %knownIds = map{${$_}[0] => 1} getKnownSimfiles;
  foreach my $entry(@entries){
    my $id = ${$entry}[0];
    if(not defined $knownIds{$id}){
      my $str = entryToString $entry;
      system "echo \"$str\" >> $zeniusFile";
      print "$str\n";
    }
  }
}

sub printKnownSimfiles(){
  my @knownSimfiles = getKnownSimfiles;
  my %simfileEpochs = map {${$_}[0] => entryEpoch $_} @knownSimfiles;
  @knownSimfiles = sort {
    $simfileEpochs{${$a}[0]} cmp $simfileEpochs{${$b}[0]}
  } @knownSimfiles;
  for my $entry(@knownSimfiles){
    print entryToString($entry) . "\n";
  }
}

sub fetchSimfile($){
  my $id = shift;
  my $url = ''
   . "http://zenius-i-vanisher.com/v5.2/download.php?"
   . "type=ddrsimfile&simfileid=$id";
  print "$url\n";
  system "wget", "--content-disposition", $url;
}

sub getKnownSimfiles(){
  my @entries;
  foreach my $line(`cat $zeniusFile 2>/dev/null`){
    my $entry = entryFromString $line;
    if(defined $entry){
      push @entries, $entry;
    }else{
      print STDERR "Malformed entry: $entry\n";
    }
  }
  return @entries;
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
  for my $entry(@entries){
    ${$entry}[3] = getAbsoluteDate ${$entry}[3];
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

sub entryEpoch($){
  my $date = ${shift()}[3];
  my $epoch = `date -d "$date" +\%s`;
  chomp $epoch;
  return $epoch;
}

sub entryToString($){
  my ($id, $name, $category, $date) = @{shift()};
  return "$id | $name | $category | $date";
}

sub entryFromString($){
  my $str = shift;
  if($str =~ /^(\d+) \| (.*?) \| (.*?) \| (.*?)\n?$/){
    return [$1, $2, $3, $4];
  }else{
    return undef;
  }
}

&main(@ARGV);
