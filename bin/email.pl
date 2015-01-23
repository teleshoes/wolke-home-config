#!/usr/bin/perl
use strict;
use warnings;
use Mail::IMAPClient;
use IO::Socket::SSL;

sub getUnreadHeaders($);
sub getClient($);
sub getSocket($);
sub readSecrets();

my $secretsFile = "$ENV{HOME}/.secrets";
my @configKeys = qw(user password server port folder);

my @headerFields = qw(Date Subject From);

my $settings = {
  Peek => 1,
  Uid => 1,
};

sub main(@){
  my $accounts = readSecrets();
  for my $accName(sort keys %$accounts){
    my $unread = getUnreadHeaders $$accounts{$accName};
    for my $uid(sort keys %$unread){
      my $hdr = $$unread{$uid};
      print "$accName $uid $$hdr{Date} $$hdr{Subject}\n"
    }
  }
}

sub getUnreadHeaders($){
  my $acc = shift;
  my $c = getClient $acc;

  if(not defined $c or not $c->IsAuthenticated()){
    warn "Could not authenticate $$acc{name} {$$acc{user}\n";
    return;
  }
  my @folders = $c->folders($$acc{folder});
  if(@folders != 1){
    warn "Error getting folder $$acc{folder}\n";
    return;
  }

  my $f = $folders[0];
  $c->select($f);

  my $unread = {};
  for my $uid($c->unseen){
    $$unread{$uid} = {uid => $uid};
    my $hdr = $c->parse_headers($uid, @headerFields);
    for my $field(@headerFields){
      $$unread{$uid}{$field} = ${$$hdr{$field}}[0];
    }
  }


  return $unread;
}

sub getClient($){
  my $acc = shift;
  return Mail::IMAPClient->new(
    Socket   => getSocket($acc),
    User     => $$acc{user},
    Password => $$acc{password},
    %$settings,
  );
}

sub getSocket($){
  my $acc = shift;
  return IO::Socket::SSL->new(
    PeerAddr => $$acc{server},
    PeerPort => $$acc{port},
  );
}


sub readSecrets(){
  my @lines = `cat $secretsFile 2>/dev/null`;
  my $accounts = {};
  my $okConfigKeys = join "|", @configKeys;
  for my $line(@lines){
    if($line =~ /^email\.(\w+)\.($okConfigKeys)\s*=\s*(.+)$/){
      $$accounts{$1} = {} if not defined $$accounts{$1};
      $$accounts{$1}{$2} = $3;
    }
  }
  for my $accName(keys %$accounts){
    my $acc = $$accounts{$accName};
    $$acc{name} = $accName;
    for my $key(sort @configKeys){
      die "Missing '$key' for '$accName' in $secretsFile\n" if not defined $$acc{$key};
    }
  }
  return $accounts;
}

&main(@ARGV);
