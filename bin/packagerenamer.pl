#!/usr/bin/perl
use strict;
use warnings;

#Takes a filename and returns the content as a large string
sub slurp($){
	my $infile = shift;
	local($/, *INFH);
	open INFH, $infile or
		die "Error opening $infile for reading\n";
	my $content = <INFH>;
	close INFH;
	return $content;
}

sub writeFile($$){
  my $fname = shift;
  my $content = shift;
  open OUTFH,  "> $fname" or die "trying to open $fname for writing";
  print OUTFH $content;
}


my $file = shift;
my $content = slurp $file;
$content =~ s/com\.escribeonline\.escribe/com.escribe.ehr/g;
$content =~ s/com\.escribeonline\.test/com.escribe.test/g;
$content =~ s/com\/escribeonline\/escribe/com\/escribe\/ehr/g;
writeFile $file, $content;

