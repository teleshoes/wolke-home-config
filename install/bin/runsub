#!/usr/bin/perl
use strict;
use warnings;
use lib `dirname $0 | tr -d '\n'`;
use ScriptScript;

sub main(@){
    die "Usage: SUB [ARG ARG ..]\n" if @_ == 0;
    my $sub = shift;
    my $subRef;
    eval{ $subRef = \&{$sub} };
    \&{$subRef}(@_);
}
&main(@ARGV);
