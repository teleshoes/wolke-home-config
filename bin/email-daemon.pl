#!/usr/bin/perl
use strict;
use warnings;
use Time::HiRes qw(sleep time);
use POSIX ":sys_wait_h";

my $cmd = "/opt/qtemail/bin/email.pl --update --folder=inbox";
my $delayMs = 30 * 1000;
my $maxTimeoutMs = 60 * 1000;

sub testPing();

sub main(@){
  print "\c[];qtemail-daemon\a";
  #sleep $delayMs / 1000.0;
  while(1){
    if(testPing){
      my $start = time;
      my $pid = fork();
      if($pid == 0){
        system "date";
        exec $cmd;
      }elsif($pid > 0){
        my $waitpid = 0;
        my $success = 1;
        while($waitpid >= 0){
          $waitpid = waitpid($pid, WNOHANG);
          if((time - $start) > ($maxTimeoutMs / 1000.0)){
            print STDERR "\n\n\n!!!!!!\nFATAL TIME OUT\nkilling $pid\n\n\n";
            system "kill", "-9", $pid;
            waitpid $pid, WNOHANG;
            system "kill", "-9", $pid;
            waitpid $pid, WNOHANG;
            $success = 0;
            last;
          }
          sleep 0.5;
        }
        print STDERR "\n\n\n" . ($success ? "success" : "failure") . "\n\n\n";
      }else{
        die "error forking\n";
      }
    }else{
      print STDERR "\n\n\ncould not ping, skipping email update\n\n\n";
    }
    sleep $delayMs / 1000.0;
  }
}

sub testPing(){
  my $out = `execPing --attempts=5`;
  if($? == 0 and $out =~ /^ok:/){
    return 1;
  }else{
    return 0;
  }
}

&main(@ARGV);
