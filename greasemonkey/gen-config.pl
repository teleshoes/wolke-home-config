#!/usr/bin/perl
use strict;
use warnings;

use Time::HiRes qw(stat);

my $scripts = [
  ["verizon_password",    "http://192.168.1.1/*",                                     ""],
  ["mohela_login",        "https://www.mohela.com/DL/secure/account/loginStep1.aspx", ""],
  ["uas_login",           "https://www.uasecho.com/",                                 ""],
  ["mohela_login_step_2", "https://www.mohela.com/DL/secure/account/loginStep2.aspx", ""],
  ["ups_tracking",        "*ups.com*track*",                                          ""],
  ["flattr_login",        "https://flattr.com/login",                                 ""],
  ["multiverse",          "http://amultiverse.com/*",                                 "hide scenes from a multiverse header"],
  ["boltbus",             "https://www.boltbus.com/*?*",                              "sets up boston=&gt;ny"],
  ["gobus",               "https://gobuses.com/*?gm-*",                               "boston&lt;=&gt;nyc"],
  ["chase",               "https://chaseonline.chase.com/Logon*",                     "chase logon"],
  ["github-username",     "https://github.com/*",                                     ""],
];

sub main(@){
  my $emptySha1 = `echo -n | sha1sum | sed 's/\\s*-\$//'`;
  chomp $emptySha1;

  my $xml = "<UserScriptConfig>\n";
  for my $s(@$scripts){
    my ($dir, $include, $desc) = @$s;
    my $uuid = `uuid`;
    chomp $uuid;

    my $file = "$dir.user.js";
    die "missing file: $dir/$file\n" if not -f "$dir/$file";

    my $name = $dir;
    $name =~ s/_/ /g;

    my $hasSecret = -f "$dir/secret.js";

    my $mtime = int((stat "$dir/$file")[9] * 1000);

    $xml .= ""
      . "\t<Script"
      . " basedir=\"$dir\""
      . " checkRemoteUpdates=\"1\""
      . " dependhash=\"$emptySha1\""
      . " description=\"$desc\""
      . " enabled=\"true\""
      . " filename=\"$file\""
      . " installTime=\"$mtime\""
      . " modified=\"$mtime\""
      . " name=\"$name\""
      . " namespace=\"www.teleshoes.org\""
      . " noframes=\"false\""
      . " runAt=\"document-end\""
      . " updateMetaStatus=\"unknown\""
      . " uuid=\"$uuid\""
      . " version=\"null\""
      . " updateurl=\"null\""
      . ">\n"
      . "\t\t<Grant>none</Grant>\n"
      . "\t\t<Include>$include</Include>\n"
      . ($hasSecret ? "\t\t<Require filename=\"secret.js\"/>\n" : "")
      . "\t</Script>\n"
      ;
  }
  $xml .= "</UserScriptConfig>\n";
  chomp $xml; #because everyone sucks forever

  system "mv", "config.xml", "config.xml.bak." . time;

  print $xml;

  open FH, "> config.xml" or die "Could not write config.xml\n";
  print FH $xml;
  close FH;
  print "\n(backed up and re-generated config.xml)\n";
}

&main(@ARGV);
