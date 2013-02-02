#!/usr/bin/perl
#
# Reformat Erlang code so that variable names
# are in format MultiWordName instead of
# Multi_word_name
#

use strict;
use File::Find;
use File::Temp qw/ tempfile tempdir /;

my $n_changes;

find(\&reformat, '.');

sub reformat
{
	my $filename = $_;
	my ($tfh, $tempfilename);
	my $data;

	if ($filename =~ m/\.(erl|asciidoc)$/)
	{
		(undef, $tempfilename) = tempfile("erlangXXXXXXXX", DIR => ".", OPEN => 0);
		$n_changes = 0;
		open INFILE, $filename;
		open OUTFILE, ">$tempfilename";
		while ($data = <INFILE>)
		{
			$data =~ s/\b([A-Z][a-zA-Z0-9]*_\w*)\b/recase($1)/eg;
			print OUTFILE $data;
		}
		close INFILE;
		close OUTFILE; 
		if ($n_changes > 0)
		{
			print STDERR "Changed file $filename\n";
			rename($filename, "$filename.bak");
			rename($tempfilename, $filename);
		}
		else
		{
			unlink $tempfilename;
		}
	}
}

sub recase
{
	my $data = shift;
	$n_changes++;
	$data =~ s/_([a-z])/uc($1)/eg;
	return $data;
}

		
		

