#!/usr/bin/perl
use strict;
use warnings;
use Time::HiRes qw(time);

my $s3EhrCfgFile = "/opt/escribe/conf/s3-ehr.cfg";
my $serialNumber = "GAKT00016063";

sub fetchAmazonSessionCfg($);
sub readS3Cfg($);
sub writeS3Cfg($$);
sub run(@);
sub procLines(@);

sub main(@){
  die "Usage: $0 TOKEN_CODE\n" if @_ != 1 or $_[0] !~ /^\d{6}$/;
  my $tokenCode = shift;

  my $amazonCfg = fetchAmazonSessionCfg $tokenCode;
  my $s3Cfg = readS3Cfg $s3EhrCfgFile;

  $s3Cfg = {%$s3Cfg, %$amazonCfg};
  writeS3Cfg $s3EhrCfgFile, $s3Cfg;
}

sub fetchAmazonSessionCfg($){
  my $tokenCode = shift;

  my @lines = procLines "aws",
    "sts", "get-session-token",
    "--serial-number", $serialNumber,
    "--token-code", $tokenCode
  ;

  my $cfg = {};
  for my $line(@lines){
    $$cfg{secretKey} = $1 if $line =~ /"SecretAccessKey"\s*:\s*"([^"]+)"/;
    $$cfg{accessKey} = $1 if $line =~ /"AccessKeyId"\s*:\s*"([^"]+)"/;
    $$cfg{sessionToken} = $1 if $line =~ /"SessionToken"\s*:\s*"([^"]+)"/;
  }

  die "Error running aws\n" if
    not defined $$cfg{secretKey} or
    not defined $$cfg{accessKey} or
    not defined $$cfg{sessionToken};

  return $cfg;
}

sub readS3Cfg($){
  my $file = shift;
  my @lines = procLines "cat", $file;
  my $cfg = {};
  for my $line(@lines){
    $$cfg{userName} = $1 if $line =~ /^\s*user_name\s*=\s*(.*?)\s*$/;
    $$cfg{bucketName} = $1 if $line =~ /^\s*bucket_name\s*=\s*(.*?)\s*$/;
    $$cfg{accessKey} = $1 if $line =~ /^\s*access_key\s*=\s*(.*?)\s*$/;
    $$cfg{secretKey} = $1 if $line =~ /^\s*secret_key\s*=\s*(.*?)\s*$/;
    $$cfg{sessionToken} = $1 if $line =~ /^\s*sessionToken\s*=\s*(.*?)\s*$/;
  }

  die "Malformed $file\n" if
    not defined $$cfg{userName} or
    not defined $$cfg{bucketName} or
    not defined $$cfg{accessKey} or
    not defined $$cfg{secretKey};

  return $cfg;
}

sub writeS3Cfg($$){
  my ($file, $cfg) = @_;
  my $millis = int(time * 1000);
  my $bakFile = "/tmp/s3-mfa-bak-$millis";
  run "cp", $file, $bakFile;

  my $config = ''
    . "user_name = $$cfg{userName}\n"
    . "bucket_name = $$cfg{bucketName}\n"
    . "access_key = $$cfg{accessKey}\n"
    . "secret_key = $$cfg{secretKey}\n"
    . "session_token = $$cfg{sessionToken}\n"
    ;

  open FH, "> $file" or die "Could not write to $file\n";
  print FH $config;
  close FH;
}

sub run(@){
  print "@_\n";
  system @_;
}

sub procLines(@){
  print "@_\n";
  open FH, "-|", @_ or die "Couldnt run @_\n";
  my @lines = <FH>;
  close FH;
  return @lines;
}

&main(@ARGV);
